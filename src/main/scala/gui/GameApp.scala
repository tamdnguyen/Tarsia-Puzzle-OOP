package gui

import gui._
import engine._
import engine.grid._
import scala.swing._
import scala.swing.event.{MouseMoved, MouseClicked, MousePressed, MouseDragged, MouseReleased, ButtonClicked}
import java.awt.{Color, BasicStroke, GridBagLayout, GridBagConstraints}
import java.awt.geom.Line2D
import java.io.File
import javax.swing.{BoxLayout, BorderFactory, JFileChooser, JOptionPane}
import javax.swing.filechooser.FileNameExtensionFilter


object GameApp extends SimpleSwingApplication:

  // Set the triangle edge length
  engine.grid.grid.edgeLength = boardHexagonSize / 4

  
  // Define the game engine of the GUI
  val game: Game = new Game()
  game.newGame()
  // game.gameBoard.generateSolution()


  // Define the main window
  def top: MainFrame = new MainFrame:
    title = "Triangle Puzzle"

    // Set minimum size and position to center of the screen
    minimumSize = new Dimension(windowWidth, windowHeight)
    preferredSize = new Dimension(windowWidth, windowHeight)
    maximumSize = new Dimension(windowWidth, windowHeight)
    resizable = false
    location = new swing.Point(
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.width - windowWidth) / 2, 
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.height - windowHeight) / 2)
    
    // Create a label to display the status of the game
    val statusLabel = new Label(s"Match the edges with same color shade together. ${game.status()}")

    // Create the bottom panel with save, load, and solve buttons
    val newGameBtn = new Button("New Game")
    val saveGameBtn = new Button("Save Game")
    val loadGameBtn = new Button("Load Game")
    val solveGameBtn = new Button("Automatic Solver")
    val bottomPanel = new FlowPanel:
      contents += newGameBtn
      contents += saveGameBtn
      contents += loadGameBtn
      contents += solveGameBtn

    // Create the left panel with a hexagon in the center
    val leftPanel = new BoardPanel(game.gameBoard):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.PINK.darker().darker().darker()

    // Create the right panel with a hexagon in the center
    val rightPanel = new BoardPanel(game.waitingBoard):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.PINK.darker().darker().darker()

    // add contents to the frame
    contents = new BorderPanel:
      layout(leftPanel) = BorderPanel.Position.West
      layout(rightPanel) = BorderPanel.Position.East
      layout(bottomPanel) = BorderPanel.Position.South
      layout(statusLabel) = BorderPanel.Position.North


    // variable to store mouse movement information
    var dragStartPoint: Option[swing.Point] = None // starting point of a drag movement
    var dragEndPoint: Option[swing.Point] = None // ending point of a drag movement
    var sourceStartDrag: Option[Component] = None // the panel where the drag movement started
    var clickPoint: Option[swing.Point] = None // the point where user right clicked mouse

    // listen to any mouse movement on gameBoard and waitingBoard
    listenTo(leftPanel.mouse.moves, leftPanel.mouse.clicks)
    listenTo(rightPanel.mouse.moves, rightPanel.mouse.clicks)
    reactions += {
      // handle right-click
      case e: MouseClicked if e.peer.getButton() == java.awt.event.MouseEvent.BUTTON3 => 
          clickPoint = Some(e.point)
          if e.source == leftPanel then
            this.clickInGameBoard()
          else if e.source == rightPanel then
            this.clickInWaitingBoard()
      // handle left-click drag
      case e: MousePressed if e.peer.getButton() == java.awt.event.MouseEvent.BUTTON1 => 
          dragStartPoint = Some(e.point)
          sourceStartDrag = Some(e.source)
      case e: MouseDragged => 
        dragEndPoint = Some(e.point)
        val feedback = sourceStartDrag match
          case Some(component) if component == leftPanel => this.feedbackFromGameBoard()
          case Some(component) if component == rightPanel => this.feedbackFromWaitingBoard()
          case _ => ""
        statusLabel.text = feedback
      case e: MouseReleased if e.peer.getButton() == java.awt.event.MouseEvent.BUTTON1 => 
        sourceStartDrag match
          case Some(component) if component == leftPanel => this.moveFromGameBoard()
          case Some(component) if component == rightPanel => this.moveFromWaitingBoard()
          case _ => 
    }


    /**
      * Select the GridPos and TriTile according to the startpoint and endpoint
      * of mouse movement starting in gameBoard.
      *
      * @return tuple (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd)
      */
    def fromGameBoard() =
      var boardEnd: Board = game.gameBoard
      val (gridPosStart, tileStart): (Option[GridPos], Option[TriTile]) = dragStartPoint match
        case Some(point) => 
          game.gameBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
        case None => (None, None)
      val (gridPosEnd, tileEnd): (Option[GridPos], Option[TriTile]) = dragEndPoint match
        case Some(point) => 
          if (point.x >= 0 && point.x < 600) &&
             (point.y >= 0 && point.y <= 600) then
            game.gameBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
          else if (point.x >= 600 && point.x <= 1200) &&
                  (point.y >= 0 && point.y <= 600) then
            boardEnd = game.waitingBoard
            game.waitingBoard.pickTile(engine.grid.Point(point.x - 600, point.y).shiftGUItoEngine(centerX, centerY))
          else
            (None, None)
        case None => (None, None)
      (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd)


    /**
      * Live feedback along with mouse movement to show user
      * where are they dragging the tile to.
      *
      * @return String shown in GameApp statusLabel
      */
    def feedbackFromGameBoard(): String =
      val start = dragStartPoint.get
      val end = dragEndPoint.get
      s"From game board location (${start.x}, ${start.y}) to location (${end.x}, ${end.y})"


    /**
      * Perform a moveTile() from gameBoard to another board.
      * 
      * If successful, repaint the GUI.
      * 
      * Update game label about success of the move.
      */
    def moveFromGameBoard() =
      val (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd) = this.fromGameBoard()
      (gridPosStart, gridPosEnd) match
        case (Some(pos1), Some(pos2)) => 
          if game.gameBoard.moveTile(boardEnd, pos1, pos2) then
            game.advance()
            this.repaintGUI()
            this.checkComplete()
            statusLabel.text = s"Successful tile move. ${game.status()}"
          else
            statusLabel.text = s"Unsuccessful tile move. ${game.status()}"
        case _ =>
          statusLabel.text = s"Unsuccessful tile move. ${game.status()}"


    /**
      * Select the GridPos and TriTile according to the startpoint and endpoint
      * of mouse movement starting in waitingBoard.
      *
      * @return tuple (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd)
      */
    def fromWaitingBoard() =
      var boardEnd: Board = game.waitingBoard
      val (gridPosStart, tileStart): (Option[GridPos], Option[TriTile]) = dragStartPoint match
        case Some(point) => 
          game.waitingBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
        case None => (None, None)
      val (gridPosEnd, tileEnd): (Option[GridPos], Option[TriTile]) = dragEndPoint match
        case Some(point) => 
          if (point.x >= 0 && point.x <= 600) &&
             (point.y >= 0 && point.y <= 600) then
            game.waitingBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
          else if (point.x >= -600 && point.x < 0) &&
                  (point.y >= 0 && point.y <= 600) then
            boardEnd = game.gameBoard
            game.gameBoard.pickTile(engine.grid.Point(point.x + 600, point.y).shiftGUItoEngine(centerX, centerY))
          else
            (None, None)
        case None => (None, None)
      (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd)


    /**
      * Live feedback along with mouse movement to show user
      * where are they dragging the tile to.
      *
      * @return String shown in GameApp statusLabel
      */
    def feedbackFromWaitingBoard(): String =
      val start = dragStartPoint.get
      val end = dragEndPoint.get
      s"From waiting board location (${start.x}, ${start.y}) to location (${end.x}, ${end.y})"


    /**
      * Perform a moveTile() from waitingBoard to another board.
      * 
      * If successful, repaint the GUI.
      * 
      * Update game label about success of the move.
      */
    def moveFromWaitingBoard() =
      val (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd) = this.fromWaitingBoard()
      (gridPosStart, gridPosEnd) match
        case (Some(pos1), Some(pos2)) => 
          if game.waitingBoard.moveTile(boardEnd, pos1, pos2) then
            game.advance()
            this.repaintGUI()
            this.checkComplete()
            statusLabel.text = s"Successful tile move. ${game.status()}"
          else
            statusLabel.text = s"Unsuccessful tile move. ${game.status()}"
        case _ =>
          statusLabel.text = s"Unsuccessful tile move. ${game.status()}"


    /**
      * Perform a rotation to a TriTile on gameBoard at position clickPoint.
      */
    def clickInGameBoard() =
      val (_, wrappedTile): (Option[GridPos], Option[TriTile]) = clickPoint match
        case Some(point) => 
          game.gameBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
        case None => (None, None)
      wrappedTile match
        case Some(tile) => 
          tile.rotateClockwise()
          game.advance()
          statusLabel.text = s"Successful tile rotate in game board. ${game.status()}"
        case None => 


    /**
      * Perform a rotation to a TriTile on waitingBoard at position clickPoint.
      */
    def clickInWaitingBoard() =
      val (_, wrappedTile): (Option[GridPos], Option[TriTile]) = clickPoint match
        case Some(point) => 
          game.waitingBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
        case None => (None, None)
      wrappedTile match
        case Some(tile) => 
          tile.rotateClockwise()
          game.advance()
          statusLabel.text = s"Successful tile rotate in waiting board. ${game.status()}"
        case None => 


    /**
      * Method to re-draw both gameBoard and waitingBoard.
      */
    def repaintGUI() =
      leftPanel.repaint()
      rightPanel.repaint()


    /**
      * If the game is completed, show a dialog to notify the player.
      */
    def checkComplete() =
      if (game.gameBoard.isCompleted) then
        Dialog.showMessage(
          title = "Puzzle Solved",
          message = "Congratulations! You have won the game!",
          parent = null
        )
    
      
    /**
      * Update the Board object inside the BoardPanel in GameApp.
      */
    def updatePanel() =
      leftPanel.board = game.gameBoard
      rightPanel.board = game.waitingBoard


    // add functionality to newGame button
    newGameBtn.reactions += {
      case ButtonClicked(_) =>
        // Ask the user if they want to save the game first
        val saveOption = JOptionPane.showConfirmDialog(
          null,
          "Do you want to save the game before starting a new game?",
          "New Game",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE
        )
        saveOption match // User wants to save the game
          case JOptionPane.YES_OPTION => // Save the game and start a new game
            saveGameBtn.doClick() // simulate a click on the save button
            newGame()
          case JOptionPane.NO_OPTION => // User does not want to save the game
            newGame()
          case JOptionPane.CANCEL_OPTION => // User cancelled the operation, do nothing

        this.updatePanel() 
        this.repaintGUI() 
        this.checkComplete()
        def newGame() =
          game.newGame()
          statusLabel.text = s"Welcome to a new game! Match the edges with same color shade together"
    }

    // add functionality to saveGame button
    saveGameBtn.reactions += {
      case ButtonClicked(_) =>
        // create a file chooser dialog
        val fileChooser = new JFileChooser()
        val defaultDirectory = new File(System.getProperty("user.dir") + "/src/main/scala/gui/data/")
        fileChooser.setCurrentDirectory(defaultDirectory)
        fileChooser.setDialogTitle("Save Game")
        fileChooser.setFileFilter(new FileNameExtensionFilter("JSON files (*.json)", "json"))
        fileChooser.setApproveButtonText("Save")
        
        // show the dialog and wait for user input
        val result = fileChooser.showSaveDialog(null)
        if result == JFileChooser.APPROVE_OPTION then
          // get the selected file and directory
          var selectedFile = fileChooser.getSelectedFile()
          if (!selectedFile.getName().toLowerCase().endsWith(".json")) then
            selectedFile = new File(selectedFile.getPath() + ".json")
          val directory = selectedFile.getParentFile()
          
          if (selectedFile.exists()) then
            val option = JOptionPane.showConfirmDialog(
              null,
              "The file already exists. Do you want to overwrite it?",
              "Save Game",
              JOptionPane.YES_NO_OPTION,
              JOptionPane.WARNING_MESSAGE
            )

          // prompt the user to enter the filename
          val filename = selectedFile.getName().stripSuffix(".json")
          
          // save the game to the selected file
          game.saveGame(new File(directory, s"$filename.json").getPath())
          statusLabel.text = s"The game has been saved!"
    }

    // add functionality to loadGame button
    loadGameBtn.reactions += {
      case ButtonClicked(_) =>
        // Ask the user if they want to save the game first
        val saveOption = JOptionPane.showConfirmDialog(
          null,
          "Do you want to save this game before loading another game?",
          "Load Game",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE
        )
        saveOption match // User wants to save the game
          case JOptionPane.YES_OPTION => // Save the game and start a new game
            saveGameBtn.doClick() // simulate a click on the save button
            load()
          case JOptionPane.NO_OPTION => // User does not want to save the game
            load()
          case JOptionPane.CANCEL_OPTION => // User cancelled the operation, do nothing

        def load() =
          // create a file chooser dialog
          val fileChooser = new JFileChooser()
          val defaultDirectory = new File(System.getProperty("user.dir") + "/src/main/scala/gui/data/")
          fileChooser.setCurrentDirectory(defaultDirectory)
          fileChooser.setDialogTitle("Load Game")
          fileChooser.setFileFilter(new FileNameExtensionFilter("JSON files (*.json)", "json"))
          fileChooser.setApproveButtonText("Load")
          
          // show the dialog and wait for user input
          val result = fileChooser.showSaveDialog(null)
          if result == JFileChooser.APPROVE_OPTION then
            // get the selected file and directory
            val selectedFile = fileChooser.getSelectedFile()
            val directory = selectedFile.getParentFile()
            
            // load the game from the selected file
            val (success, message) = game.loadGame(selectedFile.getPath())

            // display message in a popup dialog
            JOptionPane.showMessageDialog(null, 
                                          message, 
                                          "Load Game", 
                                          JOptionPane.INFORMATION_MESSAGE)
            statusLabel.text = message
            // if the game was loaded successfully, update the panel and repaint the GUI
            if (success) {
              this.updatePanel()
              this.repaintGUI()
              this.checkComplete()
            }
    }

    // add functionality to solveGame button
    solveGameBtn.reactions += {
      case ButtonClicked(_) =>
        // Ask the user if they want to save the game first
        val saveOption = JOptionPane.showConfirmDialog(
          null,
          "Do you want to save the game before seeing the solution?",
          "Reveal Solution",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE
        )
        saveOption match // User wants to save the game
          case JOptionPane.YES_OPTION => // Save the game and start a new game
            saveGameBtn.doClick() // simulate a click on the save button
            revealSolution()
          case JOptionPane.NO_OPTION => // User does not want to save the game
            revealSolution()
          case JOptionPane.CANCEL_OPTION => // User cancelled the operation, do nothing

        this.updatePanel() 
        this.repaintGUI() 
        this.checkComplete()

        def revealSolution() =
          game.emptyGameBoard()
          this.solvePuzzleGUI()
          statusLabel.text = s"Solution found!"
    }

    /**
     * Copy of solvePuzzle() from Game so that the GUI can be updated after each move.
     */
    private def solvePuzzleGUI(): Boolean =
      // Thread.sleep(1000)
      println("GameBoard tiles: " + game.gameBoard.tileList)
      println("WaitingBoard tiles: " + game.waitingBoard.tileList)
      this.repaintGUI()
      this.repaint()


      val allEmptyPos: Seq[GridPos] = game.gameBoard.emptyGridPos
      if allEmptyPos.length == 0 then
        return true
      val emptyPos = allEmptyPos(0)
      game.waitingBoard.tileList.toList.foreach( tile =>
        for i <- 0 until 3 do
          tile.rotateCounterClockwise()
          if game.gameBoard.canFit(tile, emptyPos) then
            game.waitingBoard.moveTile(game.gameBoard, tile.pos, emptyPos)
            this.repaintGUI()
            this.repaint()
            if this.solvePuzzleGUI() then 
              return true
            game.gameBoard.moveTile(game.waitingBoard,
                                    emptyPos,
                                    game.waitingBoard.emptyGridPos(0))
            this.repaintGUI()
            this.repaint()
      )
      return false   


end GameApp
package gui

import gui._
import engine._
import engine.grid._
import scala.swing._
import scala.swing.event.MouseMoved
import java.awt.{Color, BasicStroke, GridBagLayout, GridBagConstraints}
import scala.swing.event.{MouseMoved, MouseClicked, MousePressed, MouseDragged, MouseReleased}
import java.awt.geom.Line2D
import javax.swing.{BoxLayout, BorderFactory}
import java.awt.Point

object GameApp extends SimpleSwingApplication:

  // Set the triangle edge length
  engine.grid.grid.edgeLength = boardHexagonSize / 4

  
  // Define the game engine of the GUI
  val game: Game = new Game()
  // game.newGame()
  game.gameBoard.generateSolution()


  // Define the main window
  def top: MainFrame = new MainFrame:
    // Set minimum size and position to center of the screen
    minimumSize = new Dimension(windowWidth, windowHeight)
    preferredSize = new Dimension(windowWidth, windowHeight)
    maximumSize = new Dimension(windowWidth, windowHeight)
    resizable = false
    location = new swing.Point(
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.width - windowWidth) / 2, 
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.height - windowHeight) / 2)
    
    // Create a label to display the status of the game
    val statusLabel = new Label(game.status())

    // Create the bottom panel with save, load, and solve buttons
    val bottomPanel = new FlowPanel:
      contents += new Button("New Game")
      contents += new Button("Save Game")
      contents += new Button("Load Game")
      contents += new Button("Automatic Solver")

    // Create the left panel with a hexagon in the center
    val leftPanel = new BoardPanel(game.gameBoard):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.DARK_GRAY

    // Create the right panel with a hexagon in the center
    val rightPanel = new BoardPanel(game.waitingBoard):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.DARK_GRAY

    // add contents to the frame
    contents = new BorderPanel:
      layout(leftPanel) = BorderPanel.Position.West
      layout(rightPanel) = BorderPanel.Position.East
      layout(bottomPanel) = BorderPanel.Position.South
      layout(statusLabel) = BorderPanel.Position.North


    // variable store the start and end points, and source
    // of start point (leftPanel or rightPanel) of mouse drag movement
    var dragStartPoint: Option[swing.Point] = None
    var dragEndPoint: Option[swing.Point] = None
    var sourceStartDrag: Option[Component] = None

    // listen to any mouse movement on gameBoard
    listenTo(leftPanel.mouse.moves, leftPanel.mouse.clicks)
    listenTo(rightPanel.mouse.moves, rightPanel.mouse.clicks)
    reactions += {
      case e: MousePressed =>
        dragStartPoint = Some(e.point)
        sourceStartDrag = Some(e.source)
      case e: MouseDragged =>
        dragEndPoint = Some(e.point)
        val feedback = sourceStartDrag match
          case Some(component) if component == leftPanel => this.feedbackFromGameBoard()
          case Some(component) if component == rightPanel => this.feedbackFromWaitingBoard()
          case _ => ""
        statusLabel.text = feedback
      case e: MouseReleased =>
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
      val (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd) = this.fromGameBoard()
      s"From ${game.gameBoard} ${gridPosStart}, ${tileStart} to ${boardEnd} ${gridPosEnd}, ${tileEnd}"


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
            this.repaintGUI()
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
            game.gameBoard.pickTile(engine.grid.Point(point.x - 600, point.y).shiftGUItoEngine(centerX, centerY))
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
      val (gridPosStart, tileStart, gridPosEnd, tileEnd, boardEnd) = this.fromWaitingBoard()
      s"From ${game.waitingBoard} ${gridPosStart}, ${tileStart} to ${boardEnd} ${gridPosEnd}, ${tileEnd}"


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
            this.repaintGUI()
            statusLabel.text = s"Successful tile move. ${game.status()}"
          else
            statusLabel.text = s"Unsuccessful tile move. ${game.status()}"
        case _ =>
          statusLabel.text = s"Unsuccessful tile move. ${game.status()}"


    /**
      * Method to re-draw both gameBoard and waitingBoard.
      */
    def repaintGUI() =
      leftPanel.repaint()
      rightPanel.repaint()


  def runGameLoop() =
    while !game.gameBoard.isCompleted do
      val action = ???
      game.advance(action)
end GameApp
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


    var dragStartPoint: Option[swing.Point] = None
    var dragEndPoint: Option[swing.Point] = None

    listenTo(leftPanel.mouse.moves, leftPanel.mouse.clicks)
    reactions += {
      case e: MousePressed =>
        dragStartPoint = Some(e.point)
      case e: MouseDragged =>
        dragEndPoint = Some(e.point)
        statusLabel.text = this.dragFromGameBoard()
      // case e: MouseReleased =>
      //   dragStartPoint = None
      //   dragEndPoint = None
      //   statusLabel.text = "Drag action done"
    }

    def dragFromGameBoard(): String =
      val (tileStart, gridPosStart): (Option[GridPos], Option[TriTile]) = dragStartPoint match
        case Some(point) => 
          game.gameBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
        case None => 
          (None, None)

      val (tileEnd, gridPosEnd): (Option[GridPos], Option[TriTile]) = dragEndPoint match
        case Some(point) => 
          if (point.x >= 0 && point.x < 600) &&
             (point.y >= 0 && point.y < 600) then
            game.gameBoard.pickTile(engine.grid.Point(point.x, point.y).shiftGUItoEngine(centerX, centerY))
          else if (point.x >= 600 && point.x < 1200) &&
                  (point.y >= 0 && point.y < 600) then
            game.waitingBoard.pickTile(engine.grid.Point(point.x - 600, point.y).shiftGUItoEngine(centerX, centerY))
          else
            (None, None)
        case None => 
          (None, None)

      val dragFeedback: String = 
        s"from ${dragStartPoint.getOrElse("")} to ${dragEndPoint.getOrElse("")}\nFrom GameBoard ${gridPosStart}, ${tileStart} to ${gridPosEnd}, ${tileEnd}"
      dragFeedback


  def runGameLoop() =
    while !game.gameBoard.isCompleted do
      val action = ???
      game.advance(action)
end GameApp
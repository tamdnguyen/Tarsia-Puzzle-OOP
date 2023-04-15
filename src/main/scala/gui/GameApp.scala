package gui

import scala.swing._
import java.awt.{Color, BasicStroke}
import engine._
import java.awt.{GridBagLayout, GridBagConstraints}
import java.awt.event.{MouseEvent, MouseListener}
import javax.swing.BoxLayout
import java.awt.geom.Line2D
import scala.swing.event.MouseMoved

object GameApp extends SimpleSwingApplication:

  // Define constants for window size and hexagon dimensions
  val windowWidth = 1200
  val windowHeight = 700
  val boardPanelSize = 600
  val boardHexagonSize = (engine.grid.grid.edgeLength * 4).toInt
  
  // Define the game engine of the GUI
  val game: Game = new Game()
  game.newGame()

  // Define the main window
  def top: MainFrame = new MainFrame:
    // Set minimum size and position to center of the screen
    minimumSize = new Dimension(windowWidth, windowHeight)
    preferredSize = new Dimension(windowWidth, windowHeight)
    maximumSize = new Dimension(windowWidth, windowHeight)
    resizable = false
    location = new Point(
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.width - windowWidth) / 2, 
      (java.awt.Toolkit.getDefaultToolkit.getScreenSize.height - windowHeight) / 2)
    
    // Create the bottom panel with save, load, and solve buttons
    val bottomPanel = new FlowPanel:
      contents += new Button("New Game")
      contents += new Button("Save Game")
      contents += new Button("Load Game")
      contents += new Button("Automatic Solver")
      background = Color.GRAY
    
    // Create the left panel with a hexagon in the center
    val leftPanel = new BoardPanel(game.gameBoard, 300, 300):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.LIGHT_GRAY

      listenTo(mouse.moves)
      reactions += {
        case MouseMoved(_, point, _) =>
          tooltip = s"${point.x}, ${point.y}"
      }

    // Create the right panel with a hexagon in the center
    val rightPanel = new FlowPanel:
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.PINK
    
    // // Add the components to the main window
    contents = new BorderPanel:
      layout(bottomPanel) = BorderPanel.Position.South
      layout(leftPanel) = BorderPanel.Position.West
      layout(rightPanel) = BorderPanel.Position.East

end GameApp
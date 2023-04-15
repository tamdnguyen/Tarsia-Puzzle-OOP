package gui

import scala.swing._
import java.awt.{Color, BasicStroke}
import engine._
import java.awt.{GridBagLayout, GridBagConstraints}
import java.awt.event.{MouseEvent, MouseListener}
import javax.swing.{BoxLayout, BorderFactory}
import java.awt.geom.Line2D
import scala.swing.event.MouseMoved

object GameApp extends SimpleSwingApplication:

  // Define constants for sizes and positions
  val windowWidth = 1205
  val windowHeight = 700
  val boardPanelSize = 600
  val boardHexagonSize = 500
  val centerX = boardPanelSize / 2
  val centerY = centerX
  grid.grid.edgeLength = boardHexagonSize / 4

  
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
    val leftPanel = new BoardPanel(game.gameBoard, centerX, centerY):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.DARK_GRAY

    // Create the right panel with a hexagon in the center
    val rightPanel = new BoardPanel(game.waitingBoard, centerX, centerY):
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.DARK_GRAY

    contents = new BorderPanel:
      layout(leftPanel) = BorderPanel.Position.West
      layout(rightPanel) = BorderPanel.Position.East
      layout(bottomPanel) = BorderPanel.Position.South

end GameApp
package gui

import scala.swing._
import java.awt.{Color, BasicStroke}
import engine._
import java.awt.{GridBagLayout, GridBagConstraints}
import javax.swing.BoxLayout
import java.awt.geom.Line2D

object GameApp extends SimpleSwingApplication:

  // Define constants for window size and hexagon dimensions
  val windowWidth = 1200
  val windowHeight = 700
  val boardPanelSize = 600
  val boardHexagonSize = 500
  
  // Define the game engine of the GUI
  val game: Game = new Game()

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
    val leftPanel = new FlowPanel:
      preferredSize = new Dimension(boardPanelSize, boardPanelSize)
      background = Color.LIGHT_GRAY
      val centerX = preferredSize.width / 2
      val centerY = preferredSize.height / 2
      val centerPoint = new Point(centerX, centerY)
      val triHolderCorners = game.gameBoard.allElements.map(_.corners)
      val cornersShifted = triHolderCorners.map(_.map(_.shift(centerX, centerY)))


      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)
        g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(5.0f), 0.0f))
        g.setColor(Color.RED)
        g.fillOval(centerPoint.x - 5, centerPoint.y - 5, 10, 10)
        g.drawOval(centerPoint.x - 50, centerPoint.y - 50, 100, 100)
        g.setColor(Color.BLACK)

        cornersShifted.foreach{
          case Vector(p1, p2, p3) =>
            g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
            g.draw(new Line2D.Double(p2.x, p2.y, p3.x, p3.y))
            g.draw(new Line2D.Double(p3.x, p3.y, p1.x, p1.y))
          case _ => // handle case where Vector[Point] has fewer or more than three points
        }
        // cornersShifted.sliding(2).foreach { case Seq(p1, p2) =>
        //   g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
        // }

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
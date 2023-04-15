package gui

import engine._
import scala.swing._
import scala.swing.event.MouseMoved
import javax.swing.ToolTipManager
import java.awt.{Color, BasicStroke}
import java.awt.geom.Line2D


class BoardPanel(val board: Board, val centerX: Int, val centerY: Int) extends FlowPanel:

  /**
    * Set up the static TriHolder corners collection.
    */
  val holderCorners = board.allElements.map(_.corners)


  /**
    * Shift the corners coordinates according to the center point of this panel
    */
  val cornersShifted = holderCorners.map(_.map(_.shift(centerX, centerY)))


  /**
    * Add the tooltip showing the mouse position on the panel
    */
  listenTo(mouse.moves)
  reactions += {
    case e: MouseMoved =>
      peer.setToolTipText(s"${board}: (${e.point.x}, ${e.point.y})")
      repaint()
  }
  ToolTipManager.sharedInstance().setInitialDelay(0)
  ToolTipManager.sharedInstance().setDismissDelay(1000)

  /**
    * Draw the components of the panel
    */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(Color.RED)
    g.fillOval(centerX - 5, centerY - 5, 10, 10)
    g.drawOval(centerX - 50, centerY - 50, 100, 100)

    // draw the TriHolders with dashed-lines
    g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(5.0f), 0.0f))
    g.setColor(Color.BLACK)
    cornersShifted.foreach{
      case Vector(p1, p2, p3) =>
        g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
        g.draw(new Line2D.Double(p2.x, p2.y, p3.x, p3.y))
        g.draw(new Line2D.Double(p3.x, p3.y, p1.x, p1.y))
      case _ => // handle case where Vector[Point] has fewer or more than three points
    }

    // draw the edges
    g.setStroke(new BasicStroke(2.0f))
    board.tileList.foreach { triTile =>
      triTile.edges.foreach { edge =>
        val p1 = edge.p1.shift(centerX, centerY)
        val p2 = edge.p2.shift(centerX, centerY)
        val color = ColorMapper(edge.value)
        g.setColor(color)
        g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
      }
    }
  }

end BoardPanel

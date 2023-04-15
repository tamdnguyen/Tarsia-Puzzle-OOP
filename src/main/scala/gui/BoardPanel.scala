package gui

import engine._
import scala.swing._
import scala.swing.event.MouseMoved
import javax.swing.ToolTipManager
import java.awt.{Color, BasicStroke}
import java.awt.geom.Line2D


class BoardPanel(val board: Board, val centerX: Int, val centerY: Int) extends FlowPanel:

  val holderCorners = board.allElements.map(_.corners)
  val cornersShifted = holderCorners.map(_.map(_.shift(centerX, centerY)))

  listenTo(mouse.moves)
  reactions += {
    case e: MouseMoved =>
      peer.setToolTipText(s"Mouse position: (${e.point.x}, ${e.point.y})")
      repaint()
  }
  // set tooltip delay
  ToolTipManager.sharedInstance().setInitialDelay(0)
  ToolTipManager.sharedInstance().setDismissDelay(1000)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(Color.RED)
    g.fillOval(centerX - 5, centerY - 5, 10, 10)
    g.drawOval(centerX - 50, centerY - 50, 100, 100)
    g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, Array(5.0f), 0.0f))
    g.setColor(Color.BLACK)

    cornersShifted.foreach{
      case Vector(p1, p2, p3) =>
        g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y))
        g.draw(new Line2D.Double(p2.x, p2.y, p3.x, p3.y))
        g.draw(new Line2D.Double(p3.x, p3.y, p1.x, p1.y))
      case _ => // handle case where Vector[Point] has fewer or more than three points
    }
  }

end BoardPanel

package gui

import gui._
import engine._
import scala.swing._
import scala.swing.event.{MouseMoved, MouseClicked, MousePressed, MouseDragged}
import javax.swing.ToolTipManager
import java.awt.{Color, Polygon, BasicStroke}
import java.awt.geom.{Line2D, Ellipse2D, Path2D, Area}


class BoardPanel(val board: Board) extends FlowPanel:

  /**
    * Set up the static TriHolder corners collection.
    */
  val holderCorners = board.allElements.map(_.corners)


  /**
    * Shift the corners coordinates according to the center point of this panel.
    * 
    * First, we perform reflection because engine.grid increases y northward while
    * scala.swing increases y southward.
    */
  val cornersShifted = holderCorners.map(_.map(_.shiftEngineToGUI(centerX, centerY)))


  /**
    * Add the tooltip showing the mouse position on the panel
    */
  listenTo(mouse.moves, mouse.clicks)
  reactions += {
    case e: MouseMoved =>
      val pointGUI = engine.grid.Point(e.point.x, e.point.y)
      val (gridPos, tile) = board.pickTile(pointGUI.shiftGUItoEngine(centerX, centerY))
      val tileValues = tile match
        case Some(actualTile) => actualTile.values
        case _ => Vector()
      peer.setToolTipText(s"${board}: (${tileValues}, ${gridPos})")
      repaint()
  }
  ToolTipManager.sharedInstance().setInitialDelay(0)
  ToolTipManager.sharedInstance().setDismissDelay(1000)


  /**
    * Draw the components of the panel
    */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)

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

    g.setStroke(new BasicStroke(2.0f))
    board.tileList.foreach { triTile =>
      val shiftedCenter = triTile.center.shiftEngineToGUI(centerX, centerY) // center of the TriTile

      // draw the TriTile
      triTile.edges.foreach { edge =>
        val p1 = edge.p1.shiftEngineToGUI(centerX, centerY)
        val p2 = edge.p2.shiftEngineToGUI(centerX, centerY)
        val color = ColorMapper(edge.value)
        val area = new Area(new Path2D.Double() {
          moveTo(shiftedCenter.x, shiftedCenter.y)
          lineTo(p1.x, p1.y)
          lineTo(p2.x, p2.y)
          closePath()
        })
        g.setColor(color)
        g.draw(new Line2D.Double(p1.x, p1.y, p2.x, p2.y)) // draw the edges
        g.fill(area) // fill-in the TriTile portion with corresponding edge color
      }

      // paint the TriTile center point
      g.setColor(Color.WHITE)
      g.fill(new Ellipse2D.Double(shiftedCenter.x - 3, shiftedCenter.y - 3, 6, 6))
    }
  }

end BoardPanel

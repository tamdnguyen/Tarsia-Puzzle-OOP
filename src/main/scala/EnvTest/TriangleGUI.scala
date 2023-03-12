import java.awt.{Graphics, Polygon}
import javax.swing.{JFrame, JPanel}
import java.awt.geom.Line2D

object TriangleGUI extends JPanel {
  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val polygon = new Polygon(Array(100, 150, 200, 150, 100, 50), Array(0, 0, 100, 200, 200, 100), 6)
    g.drawPolygon(polygon)
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Triangle")
    frame.setSize(300, 300)
    frame.add(TriangleGUI)
    frame.setVisible(true)
  }
}

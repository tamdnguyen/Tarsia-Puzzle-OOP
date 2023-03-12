import java.awt.{Graphics, Graphics2D, Panel, Polygon}
import javax.swing.{JFrame, JPanel}
import java.awt.geom.Line2D
import math.sqrt

object Graphics2d extends JPanel {
  override def paintComponent(g: Graphics): Unit = {
    val yCoord = Array[Double](0, sqrt(3)/2, 0).map(_ * 100)
    val xCoord = Array[Double](0, 1.0/2, 1).map(_ * 100)
    val g2 = g.create.asInstanceOf[Graphics2D]
    super.paintComponent(g)
    val polygon = new Polygon(Array(100, 150, 200, 150, 100, 50), Array(0, 0, 100, 200, 200, 100), 6)
//    g2.draw(polygon)

//    g2.draw(new Line2D.Double(1.2, 40.5, 20, 30.8))

    g2.draw(new Line2D.Double(xCoord(0), yCoord(0), xCoord(1), yCoord(1)))
    g2.draw(new Line2D.Double(xCoord(1), yCoord(1), xCoord(2), yCoord(2)))
    g2.draw(new Line2D.Double(xCoord(0), yCoord(0), xCoord(2), yCoord(2)))

//    import java.awt.geom.Path2D
//    val path = new Path2D.Double
//
//    path.moveTo(xCoord(0) * 100, xCoord(0) * 100)
//    var i = 1
//    while i < xCoord.length do
//      path.lineTo(xCoord(i) * 100, yCoord(i) * 100)
//      i += 1
//    g2.draw(path)
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame("Triangle")
    frame.setSize(300, 300)
    frame.add(Graphics2d)
    frame.setVisible(true)
  }
}


package engine.grid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class PointSpec extends AnyFlatSpec with Matchers {

  "A Point" should "be equal to a Point with the same x and y coordinates" in {
    val point1 = Point(1.0, 2.0)
    val point2 = Point(1.0, 2.0)
    val point3 = Point(1.001, 2.001)

    point1 should equal (point2)
    point1 should not equal (point3)
  }

  it should "return the correct x and y differences between two points" in {
    val point1 = Point(1.0, 2.0)
    val point2 = Point(3.0, 4.0)

    point1.xDiff(point2) should equal (2.0)
    point1.yDiff(point2) should equal (2.0)
    point1.diff(point2) should equal((2.0, 2.0))
  }

  it should "return the correct Manhattan distance between two points" in {
    val point1 = Point(1.0, 2.0)
    val point2 = Point(3.0, 4.0)

    point1.distance(point2) should equal (4.0)
  }

  it should "return a textual description of the form '(x,y)'" in {
    val point = Point(1.0, 2.0)

    point.toString should equal ("(1.0,2.0)")
  }
}

package engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import engine.grid.Point

class EdgeSpec extends AnyFlatSpec with Matchers {

  "An Edge" should "have the correct matchingCoordinates behavior" in {
    val e1 = Edge(Point(0, 0), Point(1, 0), 1)
    val e2 = Edge(Point(1, 0), Point(0, 0), 2)
    val e3 = Edge(Point(0, 0), Point(2, 0), 3)
    val e4 = Edge(Point(1, 1), Point(0, 0), 4)
    val e5 = Edge(Point(0, 1), Point(0, 0), 5)
    val e6 = Edge(Point(0, 0), Point(0, 1), 6)

    e1.matchingCoordinates(e1) should be(true)
    e1.matchingCoordinates(e2) should be(true)
    e1.matchingCoordinates(e3) should be(false)
    e1.matchingCoordinates(e4) should be(false)
    e1.matchingCoordinates(e5) should be(false)
    e1.matchingCoordinates(e6) should be(false)
  }

  it should "have the correct matchingValue behavior" in {
    val e1 = Edge(Point(0, 0), Point(1, 0), 1)
    val e2 = Edge(Point(1, 0), Point(0, 0), 2)
    val e3 = Edge(Point(0, 0), Point(2, 0), 3)

    e1.matchingValue(e1) should be(true)
    e1.matchingValue(e2) should be(false)
    e1.matchingValue(e3) should be(false)
  }

  it should "have the correct equals behavior" in {
    val e1 = Edge(Point(0, 0), Point(1, 0), 1)
    val e2 = Edge(Point(1, 0), Point(0, 0), 2)
    val e3 = Edge(Point(0, 0), Point(1, 0), 1)
    val e4 = Edge(Point(1, 0), Point(0, 0), 1)

    e1 == e2 should be(false)
    e1 == e3 should be(true)
    e1 == e4 should be(true)
  }

}

package engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import engine.grid.Point
import engine.grid.grid._

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
    val e2 = Edge(Point(1, 0), Point(0, 0), 11)
    val e3 = Edge(Point(0, 0), Point(2, 0), 3)

    e1.matchingValue(e2) should be(true)
    e1.matchingValue(e3) should be(false)
  }

  it should "have the correct equals behavior" in {
    val e1 = Edge(Point(0, 0), Point(1, 0), 1)
    val e2 = Edge(Point(1, 0), Point(0, 0), 2)
    val e3 = Edge(Point(0, 0), Point(1, 0), 11)
    val e4 = Edge(Point(1, 0), Point(0, 0), 11)

    e1 == e2 should be(false)
    e1 == e3 should be(true)
    e1 == e4 should be(true)
  }

  "matchEdgeVal" should "return an Int that matches the input" in {
    val edge = Edge(Point(1, 0), Point(0, 0), 1)
    val result1 = edge.matchEdgeVal(1)
    val result2 = edge.matchEdgeVal(3)
    val result3 = edge.matchEdgeVal(22)
    val result4 = edge.matchEdgeVal(44)
    val result5 = edge.matchEdgeVal(-1)

    result1 should be (11)
    result2 should be (33)
    result3 should be (2)
    result4 should be (4)
    assert(edgeValues.contains(result5))
  }

  "randEdgeVal" should "return a random value from the list of possible edge values" in {
    val edge = Edge(Point(1, 0), Point(0, 0), 1)
    val result1 = edge.randEdgeVal()
    assert(edgeValues.contains(result1))
    val result2 = edge.randEdgeVal()
    assert(edgeValues.contains(result2))
    val result3 = edge.randEdgeVal()
    assert(edgeValues.contains(result3))
    val result4 = edge.randEdgeVal()
    assert(edgeValues.contains(result4))
  }
}

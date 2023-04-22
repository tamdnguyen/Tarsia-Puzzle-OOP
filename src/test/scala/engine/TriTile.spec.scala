package engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import math.sqrt
import engine.grid._

class TriTileSpec extends AnyFlatSpec with Matchers {
  grid.edgeLength = 1

  "A TriTile" should "initialize with the correct coordinates" in {
    val tile = TriTile(0, 1, 0)
    tile.a shouldEqual 0
    tile.b shouldEqual 1
    tile.c shouldEqual 0
  }

  it should "initialize with no owner" in {
    val tile = TriTile(0, 1, 0)
    tile.owner shouldEqual None
  }


  it should "calculate the correct corner points" in {
    val tile = TriTile(0, 1, 0)
    tile.corners shouldEqual Vector(
      Point(-0.5, 0.8660254037844386),
      Point(0.5, 0.8660254037844386),
      Point(0.0, 0.0)
    )
  }

  it should "returns the correct list of edges" in {
    val tile = TriTile(0, 1, 0)
    val p1 = Point(-0.5, 0.8660254037844386)
    val p2 = Point(0.5, 0.8660254037844386)
    val p3 = Point(0.0, 0.0)
    val e1 = Edge(p1, p2, -1)
    val e2 = Edge(p2, p3, -1)
    val e3 = Edge(p1, p3, -1)
    tile.edges shouldEqual Vector(e1, e2, e3)
  }

  it should "calculate the correct center point" in {
    val tile = TriTile(0, 1, 0)
    tile.center shouldEqual Point(0, sqrt(3)/3)
  }

  it should "determine whether it points up or not" in {
    val downTile = TriTile(0, 1, 0)
    downTile.pointsUp shouldBe false

    val upTile = TriTile(0, 1, 1)
    upTile.pointsUp shouldBe true
  }

  it should "initialize with edges of value -1" in {
    val tile = TriTile(0, 1, 0)
    tile.values shouldEqual Vector(-1,-1,-1)
  }

  it should "has correct owner after setting owner" in {
    val tile = TriTile(0, 1, 0)
    val board = GameBoard()
    tile.updateOwner(board)
    tile.owner shouldEqual Some(board)
  }

  it should "has None owner after removing owner" in {
    val tile = TriTile(0, 1, 0)
    val board = GameBoard()
    tile.updateOwner(board)
    tile.removeOwner()
    tile.owner shouldEqual None
  }

  it should "update the value of an edge that exists in the TriTile" in {
    val tile = TriTile(0,1,0)
    tile.updateEdge(tile.edges(0), 2) shouldBe true
    tile.edges should contain (Edge(tile.corners(0), tile.corners(1), 2))
    tile.updateEdge(tile.edges(2), 4) shouldBe true
    tile.edges should contain (Edge(tile.corners(0), tile.corners(2), 4))
  }

  it should "return false when given an edge that does not belong to TriTile" in {
    val tile = TriTile(0,1,0)
    val edge = Edge(tile.corners(2), tile.corners(1), -1)
    tile.updateEdge(edge, 2) shouldBe false
  }

  it should "update edge values correctly" in {
    val tile = TriTile(0, 1, 0)
    tile.updateEdgeValues(1,2,3)
    tile.values shouldEqual Vector(1,2,3)
  }

  it should "update coordinates correctly" in {
    val tile = TriTile(0, 1, 0)
    tile.updateCoords(1, 0, 0)
    tile.a shouldEqual 1
    tile.b shouldEqual 0
    tile.c shouldEqual 0
    tile.center shouldEqual Point(1.0/2, -sqrt(3)/6)
    val p1 = Point(0.0, 0.0)
    val p2 = Point(1.0, 0.0)
    val p3 = Point(0.5, -0.8660254037844386)
    val e1 = Edge(p1, p2, -1)
    val e2 = Edge(p2, p3, -1)
    val e3 = Edge(p1, p3, -1)
    tile.corners shouldEqual Vector(p1, p2, p3)
    tile.edges shouldEqual Vector(e1, e2, e3)
  }

  it should "return correct list of neighbors when there are full 3 neighbors" in {
    val board = GameBoard()
    board.initializeTile(GridPos(1,2))
    board.initializeTile(GridPos(0,2))
    board.initializeTile(GridPos(1,4))
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    tile.neighbors shouldEqual Vector(
      TriTile(0,1,1),
      TriTile(0,2,0),
      TriTile(1,1,0)
    )
  }

  it should "return correct list of neighbors when there are 2 neighbors due to position on grid edge" in {
    val board = GameBoard()
    board.initializeTile(GridPos(0,1))
    board.initializeTile(GridPos(1,1))
    val tile = TriTile(-1,2,1)
    tile.updateOwner(board)
    tile.neighbors shouldEqual Vector(
      TriTile(-1,2,0),
      TriTile(-1,1,1)
    )
  }

  it should "return correct list of neighbors when there are empty neighboring position" in {
    val board = GameBoard()
    board.initializeTile(GridPos(1,2))
    board.initializeTile(GridPos(1,4))
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    tile.neighbors shouldEqual Vector(
      TriTile(0,1,1),
      TriTile(1,1,0)
    )
  }

  it should "return correct neighbor given an edge when the neighbor exists" in {
    val board = GameBoard()
    board.initializeTile(GridPos(1,2))
    board.initializeTile(GridPos(0,2))
    board.initializeTile(GridPos(1,4))
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    tile.neighbor(tile.edges(0)) shouldEqual Some(TriTile(0,2,0))
    tile.neighbor(tile.edges(1)) shouldEqual Some(TriTile(1,1,0))
    tile.neighbor(tile.edges(2)) shouldEqual Some(TriTile(0,1,1))
  }

  it should "return None when no neighbor exists with the given edge" in {
    val board = GameBoard()
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    tile.neighbor(tile.edges(0)) shouldEqual None
  }

  it should "return correct adjacent edge given an edge when the neighbor exists" in {
    val board = GameBoard()
    board.initializeTile(GridPos(1,2))
    board.initializeTile(GridPos(0,2))
    board.initializeTile(GridPos(1,4))
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    val p1 = Point(-0.5, 0.8660254037844386)
    val p2 = Point(0.5, 0.8660254037844386)
    val p3 = Point(0.0, 0.0)
    val e1 = Edge(p1, p2, -1)
    val e2 = Edge(p2, p3, -1)
    val e3 = Edge(p1, p3, -1)
    tile.adjacentEdge(tile.edges(0)).get.matchingCoordinates(e1) shouldBe true
    tile.adjacentEdge(tile.edges(1)).get.matchingCoordinates(e2) shouldBe true
    tile.adjacentEdge(tile.edges(2)).get.matchingCoordinates(e3) shouldBe true
  }

  it should "return None for adjacent edge when the neighbor of that edge does not exist" in {
    val board = GameBoard()
    val tile = TriTile(0, 1, 0)
    tile.updateOwner(board)
    tile.adjacentEdge(tile.edges(0)) shouldEqual None
    tile.adjacentEdge(tile.edges(1)) shouldEqual None
    tile.adjacentEdge(tile.edges(2)) shouldEqual None
  }

  it should "return correct values of the edges when rotate clockwise" in {
    val tile = TriTile(0, 1, 0)
    tile.updateEdgeValues(1,2,3)
    tile.rotateClockwise()
    tile.values shouldEqual Vector(3,1,2)
    val tile2 = TriTile(2,0,0)
    tile.updateEdgeValues(4,5,6)
    tile.rotateClockwise()
    tile.values shouldEqual Vector(6,4,5)
  }

  it should "return correct values of the edges when rotate counter-clockwise" in {
    val tile = TriTile(0, 1, 0)
    tile.updateEdgeValues(1,2,3)
    tile.rotateCounterClockwise()
    tile.values shouldEqual Vector(2,3,1)
    val tile2 = TriTile(2,0,0)
    tile.updateEdgeValues(4,5,6)
    tile.rotateCounterClockwise()
    tile.values shouldEqual Vector(5,6,4)
  }


  "equals" should "return true if two TriTiles have the same values regardless of order" in {
    val x = TriTile(0,1,0)
    x.updateEdgeValues(1,2,3)
    val y = TriTile(1,1,-1)
    y.updateEdgeValues(3,1,2)
    x == y should be (true)
  }

  "equals" should "return false if two TriTiles have the different edge values" in {
    val x = TriTile(0,1,0)
    x.updateEdgeValues(1,2,3)
    val y = TriTile(1,1,-1)
    x.updateEdgeValues(3,1,3)
    x == y should be (false)
  }

}

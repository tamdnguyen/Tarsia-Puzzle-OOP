package engine

import engine.grid._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class BoardSpec extends AnyFlatSpec with Matchers {
  grid.edgeLength = 1

  "A Board" should "should have correct width, height and size" in {
    val board = new Board(7, 4)
    board.width shouldBe (7)
    board.height shouldBe (4)
    board.size shouldBe 24
  }

  it should "be filled with TriHolder" in {
    val board = new Board(7, 4)
    board.width should be (7)
    board.height should be (4)
    board.allElements.foreach(holder => holder shouldBe a [TriHolder])
    board.allPositions.length shouldBe 24
    board.allElements.length shouldBe 24
  }

  it should "initialize a tile at a given position" in {
    val board = new Board(7, 4)
    val tile = board.initializeTile(GridPos(0, 0))
    tile.owner should contain (board)
    board.numberOfTiles shouldBe 1
    board.tileList should contain (tile)
    board.elementAt(GridPos(0, 0)).tile should contain (tile)
    board.elementAt(GridPos(0,0)).isEmpty shouldBe false
  }

  it should "not initialize a tile at an occupied position" in {
    val board = new Board(7, 4)
    val tile1 = board.initializeTile(GridPos(0, 0))
    val tile2 = board.initializeTile(GridPos(0, 0))
    tile2 shouldBe tile1 // tile2 and tile1 references to the same TriTile
    board.numberOfTiles shouldBe 1
    board.tileList should contain (tile1)
    board.tileList should contain (tile2) 
  }

  it should "add an existing tile to a given position" in {
    val board = new Board(7, 4)
    val tile1 = TriTile(0,1,0)
    board.addTile(tile1, GridPos(0, 0))
    board.numberOfTiles shouldBe 1
    board.tileList should contain (tile1)
    tile1.owner should contain (board)
    tile1.a shouldBe -1
    tile1.b shouldBe 2
    tile1.c shouldBe 1
    board.elementAt(GridPos(0, 0)).tile should contain (tile1)
    board.elementAt(GridPos(0,0)).isEmpty shouldBe false
  }

  it should "remove a tile from a given position" in {
    val board = new Board(7, 4)
    val tile = board.initializeTile(GridPos(0, 0))
    board.removeTile(GridPos(0, 0))
    board.elementAt(GridPos(0, 0)).tile should not contain tile
    board.elementAt(GridPos(0, 0)).isEmpty shouldBe true
    board.numberOfTiles shouldBe 0
    board.tileList should not contain (tile)
    tile.owner shouldBe None
    tile.a shouldBe 0
    tile.b shouldBe 0
    tile.c shouldBe 0
  }

  it should "move a tile from a non-empty position to an empty position in the same board" in {
    val board = new Board(7, 4)
    val tile = board.initializeTile(GridPos(0, 0))
    board.moveTile(board, GridPos(0, 0), GridPos(1, 0)) shouldBe true
    tile.owner should contain (board)
    board.numberOfTiles shouldBe 1
    board.tileList should contain (tile)
    board.elementAt(GridPos(0, 0)).isEmpty shouldBe true
    board.elementAt(GridPos(0, 0)).tile should not contain (tile)
    board.elementAt(GridPos(1, 0)).isEmpty shouldBe false
    board.elementAt(GridPos(1, 0)).tile should contain (tile)
    tile.a shouldBe -1
    tile.b shouldBe 1
    tile.c shouldBe 2
  }

  it should "move a tile from a non-empty position to an empty position in two different boards" in {
    val board = new Board(7, 4)
    val board2 = new Board(7, 4)
    val tile = board.initializeTile(GridPos(0, 0))
    board.moveTile(board2, GridPos(0, 0), GridPos(1, 0)) shouldBe true

    board.numberOfTiles shouldBe 0
    board.tileList should not contain (tile)
    board.elementAt(GridPos(0, 0)).isEmpty shouldBe true
    board.elementAt(GridPos(0, 0)).tile should not contain (tile)
    board.elementAt(GridPos(1, 0)).isEmpty shouldBe true
    board.elementAt(GridPos(1, 0)).tile should not contain (tile)

    board2.numberOfTiles shouldBe 1
    board2.tileList should contain (tile)
    board2.elementAt(GridPos(0, 0)).isEmpty shouldBe true
    board2.elementAt(GridPos(0, 0)).tile should not contain (tile)
    board2.elementAt(GridPos(1, 0)).isEmpty shouldBe false
    board2.elementAt(GridPos(1, 0)).tile should contain (tile)

    tile.owner should contain (board2)
    tile.a shouldBe -1
    tile.b shouldBe 1
    tile.c shouldBe 2
  }

  it should "return false moving a tile from an empty position to another empty position in the same board" in {
    val board = new Board(7, 4)
    board.initializeTile(GridPos(1,0))
    board.moveTile(board, GridPos(0, 0), GridPos(1, 0)) shouldBe false
  }

  it should "return false moving a tile from an empty position to another empty position in two boards" in {
    val board = new Board(7, 4)
    val board2 = new Board(7, 4)
    board2.initializeTile(GridPos(1,0))
    board.moveTile(board2, GridPos(0, 0), GridPos(1, 0)) shouldBe false
  }

  it should "exchange two tiles between two non-empty positions in the same board" in {
    val board = new Board(7, 4)
    val tile1 = board.initializeTile(GridPos(0, 0))
    tile1.updateEdgeValues(1,2,3)
    val tile2 = board.initializeTile(GridPos(1, 0))
    tile2.updateEdgeValues(4,5,6)
    board.moveTile(board, GridPos(0, 0), GridPos(1, 0)) shouldBe true
    board.numberOfTiles shouldBe 2
    board.tileList should contain (tile1)
    board.tileList should contain (tile2)
    board.elementAt(GridPos(0, 0)).tile should contain (tile2)
    board.elementAt(GridPos(0, 0)).tile.get.values shouldEqual Vector(4,5,6)
    board.elementAt(GridPos(1, 0)).tile should contain (tile1)
    board.elementAt(GridPos(1, 0)).tile.get.values shouldEqual Vector(1,2,3)
  }

  it should "exchange two tiles between two non-empty positions in two boards" in {
    val board = new Board(7, 4)
    val board2 = new Board(7, 4)
    val tile1 = board.initializeTile(GridPos(0, 0))
    tile1.updateEdgeValues(1,2,3)
    val tile2 = board2.initializeTile(GridPos(1, 0))
    tile2.updateEdgeValues(4,5,6)
    board.moveTile(board2, GridPos(0, 0), GridPos(1, 0)) shouldBe true

    board.numberOfTiles shouldBe 1
    board.tileList should contain (tile2)
    board.elementAt(GridPos(0, 0)).tile should contain (tile2)
    board.elementAt(GridPos(0, 0)).tile.get.values shouldEqual Vector(4,5,6)

    board2.numberOfTiles shouldBe 1
    board2.tileList should contain (tile1)
    board2.elementAt(GridPos(1, 0)).tile should contain (tile1)
    board2.elementAt(GridPos(1, 0)).tile.get.values shouldEqual Vector(1,2,3)
  }

  it should "return the correct TriTile for a valid position on the board" in {
    val board = new Board(7,4)
    board.allPositions.foreach(board.initializeTile(_)) 
    val tile1 = board.tileList(9)
    val tile2 = board.tileList(8)
    val tile3 = board.tileList(1)
    val tile4 = board.tileList(19)
    val tile5 = board.tileList(11)
    board.pickTile(Point(0.5, 0.5)) should contain (tile1)
    board.pickTile(Point(0, 0.5)) should contain (tile2)
    board.pickTile(Point(-0.5, 1)) should contain (tile3)
    board.pickTile(Point(-1, -1)) should contain (tile4)
    board.pickTile(Point(1.999, 0.001)) should contain (tile5)
  }

  it should "return the None for an invalid position on the board" in {
    val board = new Board(7,4)
    board.pickTile(Point(0.5, 0.5)) shouldBe None
    board.pickTile(Point(0, 0.5)) shouldBe None
    board.pickTile(Point(-0.5, 1)) shouldBe None
    board.pickTile(Point(-1, -1)) shouldBe None
    board.pickTile(Point(1.999, 0.001)) shouldBe None
  }

  it should "returns correct collection of TriHolder pointing up" in {
    val board = new Board(7, 4)
    assert(board.upHolders.forall(holder => holder.pointsUp))
    board.upHolders.length shouldBe 12
  }

  it should "returns correct collection of TriHolder pointing down" in {
    val board = new Board(7, 4)
    assert(board.downHolders.forall(holder => !holder.pointsUp))
    board.downHolders.length shouldBe 12
  }
}

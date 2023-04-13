package engine

import engine.grid.grid._
import engine.grid._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._


class GameBoardSpec extends AnyFlatSpec with Matchers {

  "A GameBoard" should "initialize with 24 positions" in {
    val board = new GameBoard()
    assert(board.allElements.length == 24)
  }

  it should "have no TriTile initially" in {
    val board = new GameBoard()
    assert(board.numberOfTiles == 0)
  }

  it should "not be filled initially" in {
    val board = new GameBoard()
    assert(!board.isFilled)
  }

  "isFilled" should "return true if populated with all tiles in 24 TriHolders" in {
    val board = new GameBoard()
    board.allPositions.foreach(board.initializeTile(_))
    assert(board.isFilled)
  }

  "allMatchingEdges" should "return true if board is empty" in {
    val board = new GameBoard()
    assert(board.allMatchingEdges)
  }

  it should "return true if all adjacent edges (1 total) share the same values" in {
    val board = new GameBoard()
    val tile1 = TriTile(0,1,0)
    val tile2 = TriTile(1,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(22,2,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    assert(board.allMatchingEdges)
  }

  it should "return true if all adjacent edges share the same values" in {
    val board = new GameBoard()
    val tile1 = TriTile(-1,2,1)
    val tile2 = TriTile(-1,2,0)
    val tile3 = TriTile(-1,1,1)
    val tile4 = TriTile(0,1,1)
    val tile5 = TriTile(0,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(1,2,22)
    tile3.updateEdgeValues(33,4,1)
    tile4.updateEdgeValues(44,2,1)
    tile5.updateEdgeValues(2,3,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    board.addTile(tile3, tile3.pos)
    board.addTile(tile4, tile4.pos)
    board.addTile(tile5, tile5.pos)
    assert(board.allMatchingEdges)
  }

  it should "return false if some adjacent edges do not share the same values" in {
    val board = new GameBoard()
    val tile1 = TriTile(-1,2,1)
    val tile2 = TriTile(-1,2,0)
    val tile3 = TriTile(-1,1,1)
    val tile4 = TriTile(0,1,1)
    val tile5 = TriTile(0,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(1,2,33)
    tile3.updateEdgeValues(33,4,1)
    tile4.updateEdgeValues(44,2,1)
    tile5.updateEdgeValues(2,3,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    board.addTile(tile3, tile3.pos)
    board.addTile(tile4, tile4.pos)
    board.addTile(tile5, tile5.pos)
    assert(!board.allMatchingEdges)
  }

  "hasIdenticalTiles" should "return true if all tiles have different edge values" in {
    val board = new GameBoard()
    val tile1 = TriTile(-1,2,1)
    val tile2 = TriTile(-1,2,0)
    val tile3 = TriTile(-1,1,1)
    val tile4 = TriTile(0,1,1)
    val tile5 = TriTile(0,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(1,2,22)
    tile3.updateEdgeValues(33,4,1)
    tile4.updateEdgeValues(44,2,1)
    tile5.updateEdgeValues(2,3,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    board.addTile(tile3, tile3.pos)
    board.addTile(tile4, tile4.pos)
    board.addTile(tile5, tile5.pos)
    assert(!board.hasIdenticalTiles)
  }

  it should "return false if some tiles have same edge values" in {
    val board = new GameBoard()
    val tile1 = TriTile(-1,2,1)
    val tile2 = TriTile(-1,2,0)
    val tile3 = TriTile(-1,1,1)
    val tile4 = TriTile(0,1,1)
    val tile5 = TriTile(0,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(22,2,3)
    tile3.updateEdgeValues(33,4,1)
    tile4.updateEdgeValues(44,2,1)
    tile5.updateEdgeValues(2,3,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    board.addTile(tile3, tile3.pos)
    board.addTile(tile4, tile4.pos)
    board.addTile(tile5, tile5.pos)
    assert(board.hasIdenticalTiles)
  }

  "isCompleted" should "return true for a valid board configuration" in {
    val board = new GameBoard()
    board.allPositions.foreach(board.initializeTile(_))
    val tiles = board.tileList
    tiles(0).updateEdgeValues(44,1,33)
    tiles(1).updateEdgeValues(11,11,11)
    tiles(2).updateEdgeValues(1,4,22)
    tiles(3).updateEdgeValues(22,33,44)
    tiles(4).updateEdgeValues(3,3,4)
    tiles(5).updateEdgeValues(4,22,11)
    tiles(6).updateEdgeValues(3,44,2)
    tiles(7).updateEdgeValues(4,11,3)
    tiles(8).updateEdgeValues(2,1,1)
    tiles(9).updateEdgeValues(11,22,4)
    tiles(10).updateEdgeValues(44,2,2)
    tiles(11).updateEdgeValues(22,44,44)
    tiles(12).updateEdgeValues(1,33,11)
    tiles(13).updateEdgeValues(3,44,44)
    tiles(14).updateEdgeValues(33,22,4)
    tiles(15).updateEdgeValues(2,1,11)
    tiles(16).updateEdgeValues(44,33,11)
    tiles(17).updateEdgeValues(3,44,11)
    tiles(18).updateEdgeValues(4,1,4)
    tiles(19).updateEdgeValues(4,4,4)
    tiles(20).updateEdgeValues(44,2,22)
    tiles(21).updateEdgeValues(1,3,22)
    tiles(22).updateEdgeValues(33,1,11)
    tiles(23).updateEdgeValues(1,2,11)
    assert(board.isCompleted)
  }

  it should "return false if board configuration is not completed" in {
    val board = new GameBoard()
    val tile1 = TriTile(-1,2,1)
    val tile2 = TriTile(-1,2,0)
    val tile3 = TriTile(-1,1,1)
    val tile4 = TriTile(0,1,1)
    val tile5 = TriTile(0,1,0)
    tile1.updateEdgeValues(1,2,3)
    tile2.updateEdgeValues(22,2,3)
    tile3.updateEdgeValues(33,4,1)
    tile4.updateEdgeValues(44,2,1)
    tile5.updateEdgeValues(2,3,22)
    board.addTile(tile1, tile1.pos)
    board.addTile(tile2, tile2.pos)
    board.addTile(tile3, tile3.pos)
    board.addTile(tile4, tile4.pos)
    board.addTile(tile5, tile5.pos)
    assert(!board.isCompleted)
  }

  "randEdgeVal" should "return a random value from the list of possible edge values" in {
    val board = new GameBoard()
    val result1 = board.randEdgeVal()
    assert(edgeValues.contains(result1))
    val result2 = board.randEdgeVal()
    assert(edgeValues.contains(result2))
    val result3 = board.randEdgeVal()
    assert(edgeValues.contains(result3))
    val result4 = board.randEdgeVal()
    assert(edgeValues.contains(result4))
  }

  "Board" should "generate a valid solution" in {
    val board = new GameBoard()
    board.generateSolution()
    assert(board.isCompleted)
  }

  it should "shuffle tiles" in {
    val board = new GameBoard()
    board.generateSolution()
    val originalTiles = board.tileList.toList
    board.shuffleTiles()
    assert(board.tileList.toList != originalTiles)
  }

  it should "not be completed after shuffling tiles" in {
    val board = new GameBoard()
    board.generateSolution()
    board.shuffleTiles()
    assert(!board.isCompleted)
  }

  it should "not have identical tiles after shuffling tiles" in {
    val board = new GameBoard()
    board.generateSolution()
    board.shuffleTiles()
    assert(!board.hasIdenticalTiles)
  }
}

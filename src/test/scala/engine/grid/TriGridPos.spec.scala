package engine.grid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class TriGridPosSpec extends AnyFlatSpec with Matchers {

  "TriGridPos" should "convert to GridPos correctly" in {
    TriGridPos(-1,2,1).toGridPos should be (GridPos(0,0))
    TriGridPos(2,1,-1).toGridPos should be (GridPos(1,6))
    TriGridPos(0,0,2).toGridPos should be (GridPos(2,1))
    TriGridPos(1,-1,1).toGridPos should be (GridPos(3,2))
  }

  it should "raise key not found error for invalid TriGridPos" in {
    assertThrows[NoSuchElementException] {
      TriGridPos(1,2,3).toGridPos
    }
  }

  it should "determine whether it points up or not" in {
    val upPos = TriGridPos(0, 1, 1)
    val downPos = TriGridPos(1, -1, 1)
    upPos.pointsUp should be (true)
    downPos.pointsUp should be (false)
  }

  it should "calculate center point correctly" in {
    val pos1 = TriGridPos(0, 1, 0)
    val pos2 = TriGridPos(1, 1, 0)
    val pos3 = TriGridPos(1, 0, 0)
    pos1.center should be (Point(0, math.sqrt(3) / 3))
    pos2.center should be (Point(0.5, math.sqrt(3) / 6))
    pos3.center should be (Point(0.5, -math.sqrt(3) / 6))
  }

  it should "find all possible neighboring location correctly" in {
    val pos = TriGridPos(0,1,0)
    pos.allPossibleNeighbors should contain theSameElementsInOrderAs Seq(
      TriGridPos(0,1,1),
      TriGridPos(0,2,0),
      TriGridPos(1,1,0)
    )
    val pos1 = TriGridPos(1,0,1)
    pos1.allPossibleNeighbors should contain theSameElementsInOrderAs Seq(
      TriGridPos(0,0,1),
      TriGridPos(1,0,0),
      TriGridPos(1,-1,1)
    )
  }

  it should "find all actual neighboring location correctly (bounded by Grid)" in {
    val pos = TriGridPos(1,1,-1)
    pos.neighbors should contain theSameElementsInOrderAs Seq(
      TriGridPos(1,1,0),
      TriGridPos(1,2,-1),
      TriGridPos(2,1,-1)
    )
    val pos1 = TriGridPos(0,-1,2)
    pos1.neighbors should contain theSameElementsInOrderAs Seq(
      TriGridPos(0,0,2),
      TriGridPos(1,-1,2)
    )
    val pos2 = TriGridPos(2,0,-1)
    pos2.neighbors should contain theSameElementsInOrderAs Seq(
      TriGridPos(2,0,0),
      TriGridPos(2,1,-1)
    )
  }

  it should "compare equality correctly" in {
    val pos1 = TriGridPos(1, 2, -3)
    val pos2 = TriGridPos(1, 2, -3)
    val pos3 = TriGridPos(1, 1, -3)
    pos1 should equal (pos2)
    pos1 should not equal pos3
  }
}

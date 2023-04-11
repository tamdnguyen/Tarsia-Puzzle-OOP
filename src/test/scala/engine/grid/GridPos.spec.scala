package engine.grid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class GridPosSpec extends AnyFlatSpec with Matchers {

  "GridPos" should "convert to TriGridPos correctly" in {
    val gridPos = GridPos(0, 0)
    val triGridPos = gridPos.toTriGridPos
    triGridPos shouldEqual TriGridPos(-1,2,1)

    val gridPos2 = GridPos(1, 1)
    val triGridPos2 = gridPos2.toTriGridPos
    triGridPos2 shouldEqual TriGridPos(-1, 1, 1)

    val gridPos3 = GridPos(3,0)
    val triGridPos3 = gridPos3.toTriGridPos
    triGridPos3 shouldEqual TriGridPos(0,-1,2)
  }

  it should "raise key not found error for invalid GridPos" in {
    assertThrows[NoSuchElementException] {
      GridPos(4,4).toTriGridPos
    }
  }

  it should "calculate all possible neighbors correctly" in {
    val gridPos = GridPos(2,3)
    val neighbors = gridPos.neighbors
    neighbors should contain theSameElementsAs Seq(
      GridPos(2,2),
      GridPos(2,4),
      GridPos(3,2)
    )

    val gridPos2 = GridPos(1,5)
    val neighbors2 = gridPos2.neighbors
    neighbors2 should contain theSameElementsAs Seq(
      GridPos(1,4),
      GridPos(0,4),
      GridPos(1,6)
    )
  }

  it should "calculate neighbors correctly" in {
    val gridPos = GridPos(0,0)
    val neighbors = gridPos.neighbors
    neighbors should contain theSameElementsAs Seq(
      GridPos(0,1),
      GridPos(1,1)
    )

    val gridPos2 = GridPos(1,1)
    val neighbors2 = gridPos2.neighbors
    neighbors2 should contain theSameElementsAs Seq(
      GridPos(1,0),
      GridPos(0,0),
      GridPos(1,2)
    )

    val gridPos3 = GridPos(2,6)
    val neighbors3 = gridPos3.neighbors
    neighbors3 should contain theSameElementsAs Seq(
      GridPos(2,5),
      GridPos(1,6)
    )
  }

  it should "compare equality correctly" in {
    val gridPos = GridPos(0, 0)
    val gridPos2 = GridPos(0, 0)
    val gridPos3 = GridPos(1, 1)
    gridPos shouldEqual gridPos2
    gridPos shouldNot equal(gridPos3)
  }

}

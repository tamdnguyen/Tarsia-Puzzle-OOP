package engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import math.sqrt
import engine.grid._

class TriHolderSpec extends AnyFlatSpec with Matchers {
  grid.edgeLength = 1

  "A TriHolder" should "point up if one vertex is above and two are below" in {
    val holder1 = new TriHolder(GridPos(0, 0))
    holder1.pointsUp shouldBe true
    val holder2 = new TriHolder(GridPos(1, 0))
    holder2.pointsUp shouldBe true
    val holder3 = new TriHolder(GridPos(1, 1))
    holder3.pointsUp shouldBe false
  }

  it should "be empty by default" in {
    val holder = new TriHolder(GridPos(0, 0))
    holder.isEmpty shouldBe true
    holder.nonEmpty shouldBe false
    holder.tile shouldBe None
  }

  it should "be able to add and remove a triangle tile" in {
    val holder = new TriHolder(GridPos(0, 0))
    val tile = TriTile(0,1,0)
    holder.addTile(tile) shouldBe true
    holder.isEmpty shouldBe false
    holder.nonEmpty shouldBe true
    holder.tile shouldBe Some(tile)
    holder.removeTile() shouldBe Some(tile)
    holder.isEmpty shouldBe true
    holder.nonEmpty shouldBe false
    holder.tile shouldBe None
  }

  it should "not be able to hold two triangle tiles at the same time" in {
    val holder = new TriHolder(GridPos(0, 0))
    val tile1 = TriTile(0, 0, 0)
    val tile2 = TriTile(1, 0, 0)
    holder.addTile(tile1) shouldBe true
    holder.addTile(tile2) shouldBe false
    holder.tile shouldBe Some(tile1)
  }

  it should "calculate the correct center point" in {
    val holder = TriHolder(GridPos(1,3))
    holder.center shouldEqual Point(0, sqrt(3)/3)
  }

  it should "calculate the correct corner points" in {
    val holder = TriHolder(GridPos(1,3))
    holder.center shouldEqual Point(0, sqrt(3)/3)
    holder.corners shouldEqual Vector(
      Point(-0.5, 0.8660254037844386),
      Point(0.5, 0.8660254037844386),
      Point(0.0, 0.0)
    )
  }

}

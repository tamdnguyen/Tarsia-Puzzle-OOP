package engine.grid

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class GridPackageSpec extends AnyFlatSpec with Matchers {

  "gridToTriGrid" should "return the correct value for each GridPos" in {
    grid.gridToTriGrid((0,0)) shouldEqual (-1,2,1)
    grid.gridToTriGrid((0,1)) shouldEqual (-1,2,0)
    grid.gridToTriGrid((1,6)) shouldEqual (2,1,-1)
    grid.gridToTriGrid((2,2)) shouldEqual (0,0,1)
    grid.gridToTriGrid((3,4)) shouldEqual (2,-1,0)
  }

  "triGridToGrid" should "return the correct value for each TriGridPos" in {
    grid.triGridToGrid((0,2,-1)) shouldEqual (0,3)
    grid.triGridToGrid((1,2,-1)) shouldEqual (0,4)
    grid.triGridToGrid((0,1,1)) shouldEqual (1,2)
    grid.triGridToGrid((2,0,-1)) shouldEqual (2,6)
    grid.triGridToGrid((2,-1,1)) shouldEqual (3,3)
  }

}

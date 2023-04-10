package engine.grid

package object grid {
  var edgeLength: Double = 1.0 // edgeLength of triangles in the hexagonal grid
  val coeff: Double = math.sqrt(3) // coefficient used for triangular calculation

  val gridToTriGrid: Map[(Int, Int), (Int, Int, Int)] =
    Map((0,0) -> (-1,2,1),
        (0,1) -> (-1,2,0),
        (0,2) -> (0,2,0),
        (0,3) -> (0,2,-1),
        (0,4) -> (1,2,-1),
        (1,0) -> (-1,1,2),
        (1,1) -> (-1,1,1),
        (1,2) -> (0,1,1),
        (1,3) -> (0,1,0),
        (1,4) -> (1,1,0),
        (1,5) -> (1,1,-1),
        (1,6) -> (2,1,-1),
        (2,0) -> (-1,0,2),
        (2,1) -> (0,0,2),
        (2,2) -> (0,0,1),
        (2,3) -> (1,0,1),
        (2,4) -> (1,0,0),
        (2,5) -> (2,0,0),
        (2,6) -> (2,0,-1),
        (3,0) -> (0,-1,2),
        (3,1) -> (1,-1,2),
        (3,2) -> (1,-1,1),
        (3,3) -> (2,-1,1),
        (3,4) -> (2,-1,0))

  val triGridToGrid: Map[(Int, Int, Int), (Int, Int)] =
    gridToTriGrid.map(_.swap)

}

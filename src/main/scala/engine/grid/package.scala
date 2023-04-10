package engine.grid

package object grid:
  
  /**
    * Edge length of triangles in the hexagonal grid.
    */
  var edgeLength: Double = 1.0 

  /**
    * Coefficient used for triangular calculation.
    */
  val coeff: Double = math.sqrt(3) 

  /**
    * Declaration of matching values for the edges.
    */
  val matchingEdges: Vector[(Int, Int)] = 
    Vector((1,11),
           (2,22),
           (3,33),
           (4,44))
  
  /**
    * Return a list of all possible values for triangle edges.
    */
  val edgeValues: Vector[Int] = 
    this.matchingEdges.flatMap((x,y) => Seq(x,y))

  /**
    * HashMap convert `GridPos` coordinates to `TriGridPos` coordinates.
    */
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

  /**
    * HashMap convert `TriGridPos` coordinates to `GridPos` coordinates.
    */
  val triGridToGrid: Map[(Int, Int, Int), (Int, Int)] =
    gridToTriGrid.map(_.swap)

end grid

package engine.grid

import engine.grid.grid._
import scala.annotation.targetName

/** An object of type `TriGridPos` represents 3-Tuple of integer coordinates. Such a tuple can
 * be used to reference a position on a [[TriGrid]]. 
 * 
 * If the coordinates of `TriGridPos` are valid,
 * `TriGridPos` will have a compatible `GridPos` object, and both of them refers to the same
 * position on a [[TriGrid]]
 *
 * The coordinate axes are named `a` and `b` and `c`. More details about the coordinate system
 * can be found at [[https://www.boristhebrave.com/2021/05/23/triangle-grids/#Better_Geometry_Than_Hexes Triangle Grid]]
 */
final case class TriGridPos(val a: Int, val b: Int, val c: Int):

  /** Returns the compatible GridPos coordinate on [[TriGrid]].
   *  The method here assumes that the TriGridPos coordinates are valid.
   *  */
  def toGridPos: GridPos =
    val (x, y) = triGridToGrid((this.a, this.b, this.c))
    GridPos(x, y)
  
  /** Determines whether this grid position equals the given one. This is the case if
   * the two have identical a, b, and c coordinates. */
  @targetName("equals")
  def ==(another: TriGridPos): Boolean =
    this.a == another.a &&
    this.b == another.b &&
    this.c == another.c


  /** Determines whether the triangle position points up or not
   *  (points up means one vertex is above, two other vertices are below)
   *  */
  def pointsUp: Boolean = this.a + this.b + this.c == 2


  /** Returns the center (in cartesian co-ordinates) of triangle at position (a,b,c)
   * in triangular coordinate system.
   *
   * @example {{{
   * TriGridPos(0,1,0).center() // Point(0, 0.57735) = (0, sqrt(3)/3)
   * TriGridPos(1,1,0).center() // Point(0.5, 0.288675) = (1/2, sqrt(3)/6)
   * TriGridPos(1,0,0).center() // Point(0.5, -0.288675) = (1/2, -sqrt(3)/6)
   * }}}
   */
  def center: Point =
    Point((0.5 * this.a + -0.5 * this.c) * edgeLength,
      (-coeff / 6 * this.a + coeff / 3 * this.b - coeff / 6 * this.c) * edgeLength)


  /** 
   * Returns the vector of this triangle's all possible neighbors.
   * All possible neighbors mean the presence of the neighbor is determined
   * by the grid coordinate system rules, and is not bounded by the
   * size of the grid itself.
   * 
   * The neighbors are ordered from the left-most location 
   * and in clockwise direction.
   */
  def allPossibleNeighbors: Seq[TriGridPos] =
    if this.pointsUp then
      Seq(TriGridPos(this.a - 1, this.b, this.c),
          TriGridPos(this.a, this.b, this.c - 1),
          TriGridPos(this.a, this.b - 1, this.c))
    else
      Seq(TriGridPos(this.a, this.b, this.c + 1),
          TriGridPos(this.a, this.b + 1, this.c),
          TriGridPos(this.a + 1, this.b, this.c))


  /** Returns a vector of all the neighboring positions.
    *
    * Note that an position at the grid’s edge has fewer neighbors 
    * than one in the middle. For instance, the element at (0, 0) 
    * of a 24-tiles hexagonal board only has 2 neighbors.
    * */
  def neighbors: Seq[TriGridPos] =
    def mapContainsPos(pos: TriGridPos): Boolean =
      triGridToGrid.keys.toSeq.contains((pos.a, pos.b, pos.c))
    this.allPossibleNeighbors
        .filter(mapContainsPos(_))

end TriGridPos



package engine.grid

import scala.annotation.targetName

/** An object of type `TriGridPos` represents 3-Tuple of integer coordinates. Such a tuple can
 * be used to reference a position on a [[TriGrid]]. If the coordinates of `TriGridPos` are valid,
 * `TriGridPos` will have a compatible `GridPos` object, and both of them refers to the same
 * position on a [[TriGrid]]
 *
 * The coordinate axes are named `a` and `b` and `c`. More details about the coordinate system
 * can be found at [[https://www.boristhebrave.com/2021/05/23/triangle-grids/#Better_Geometry_Than_Hexes Triangle Grid]]
 */
final case class TriGridPos(val a: Int, val b: Int, val c: Int):

  def toGridPos: GridPos = ???
  
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


  /** Returns the vector of this triangle's neighbors */
  def neighbors: Seq[TriGridPos] =
    if this.pointsUp then
      Seq(TriGridPos(this.a - 1, this.b, this.c),
           TriGridPos(this.a, this.b - 1, this.c),
           TriGridPos(this.a, this.b, this.c - 1))
    else
      Seq(TriGridPos(this.a + 1, this.b, this.c),
           TriGridPos(this.a, this.b + 1, this.c),
           TriGridPos(this.a, this.b, this.c + 1))

end TriGridPos



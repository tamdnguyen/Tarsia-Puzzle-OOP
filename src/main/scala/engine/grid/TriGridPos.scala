package engine.grid

import scala.math.{ceil, floor, sqrt}

/** An object of type `TriGridPos` represents 3-Tuple of integer coordinates. Such a tuple can
 * be used to reference a triangle on a [[Grid]].
 *
 * The coordinate axes are named `a` and `b` and `c`. More details about the coordinate system
 * can be found at [[https://www.boristhebrave.com/2021/05/23/triangle-grids/#Better_Geometry_Than_Hexes Triangle Grid]]
 */
final case class TriGridPos(val a: Int, val b: Int, val c: Int):

  private val coeff = sqrt(3)
  private var edgeLength: Double = 1

  /** Returns length of the triangle edge. */
  def edgeLegth = this.edgeLength


  /** Update the length of the triangle edge */
  def updateLength(newLength: Double) = this.edgeLength == newLength


  /** Returns the center of a this triangle in cartesian co-ordinates.
   *
   * @return [[Point]]
   */
  // TODO: add example to code documenatation
  def center: Point =
    Point((0.5 * this.a + -0.5 * this.c) * this.edgeLength,
      (-this.coeff / 6 * this.a + this.coeff / 3 * this.b - this.coeff / 6 * this.c) * this.edgeLength)


  /** Returns the center (in cartesian co-ordinates) of triangle at position (a,b,c)
   * in triangular coordinate system.
   *
   * @return [[Point]]
   */
  // TODO: add example to code documenatation
  def center(a: Int, b: Int, c: Int): Point =
    Point((0.5 * a + -0.5 * c) * this.edgeLength,
      (-this.coeff / 6 * a + this.coeff / 3 * b - this.coeff / 6 * c) * this.edgeLength)


  /** Determines whether the triangle points up or not */
  def pointsUp: Boolean = this.a + this.b + this.c == 2


  /** Returns the array of the corners' coordinates in Cartersian coordinate system.
   *
   * @return [[List[Point]]] */
  // TODO: add example in the method documentation
  // e.g. corner coordinates for tri(0,1,0), tri(1,1,0)
  def triCorners: List[Point] =
    if this.pointsUp then
      List(this.center(1 + this.a, this.b, this.c),
        this.center(this.a, this.b, 1 + this.c),
        this.center(this.a, 1 + this.b, this.c))
    else
      List(this.center(-1 + this.a, this.b, this.c),
        this.center(this.a, this.b, -1 + this.c),
        this.center(this.a, -1 + this.b, this.c))


  /** Returns the array of this triangle's neighbors */
  def neighbors: List[TriGridPos] =
    if this.pointsUp then
      List(TriGridPos(this.a - 1, this.b, this.c),
           TriGridPos(this.a, this.b - 1, this.c),
           TriGridPos(this.a, this.b, this.c - 1))
    else
      List(TriGridPos(this.a + 1, this.b, this.c),
           TriGridPos(this.a, this.b + 1, this.c),
           TriGridPos(this.a, this.b, this.c + 1))

end TriGridPos



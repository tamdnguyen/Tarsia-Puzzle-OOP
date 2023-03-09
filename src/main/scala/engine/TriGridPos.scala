package engine

import math.{ceil, floor, sqrt}

/** An object of type `TriGridPos` represents 3-Tuple of integer coordinates. Such a tuple can
  * be used to reference a triangle on a [[Grid]].
  *
  * The coordinate axes are named `a` and `b` and `c`. More details about the coordinate system
  * can be found at [[https://www.boristhebrave.com/2021/05/23/triangle-grids/#Better_Geometry_Than_Hexes Triangle Grid]]
  *
  * `TriGridPos` objects are immutable.*/
final case class TriGridPos(val a: Int, val b: Int, val c: Int):

  private val coeff = sqrt(3)
  private var edgeLength: Double = 1

  /** Returns length of the triangle edge.*/
  def edgeLegth = this.edgeLength


  /** Update the length of the triangle edge */
  def updateLength(newLength: Double) = this.edgeLength == newLength


  /** Returns the center of a given triangle in cartesian co-ordinates.
   *
   * Each unit of a, b, c moves you in the direction of one of the edges of a
   * down triangle, in linear combination.
   * Or equivalently, this function multiplies by the inverse matrix to pick_tri
   *
   * NB: This function has the nice property that if you pass in x,y,z values that
   * sum to zero (not a valid triangle), it'll return co-ordinates for the vertices of the
   * triangles.
   *
   * @return [[Point]]
   */
  // TODO: add example to code documenatation
  def center(a: Int, b: Int, c: Int): Point =
    Point((0.5 * a + -0.5 * c) * this.edgeLength,
      (-this.coeff / 6 * a + this.coeff / 3 * b - this.coeff / 6 * c) * this.edgeLength)


  /** Determines whether the triangle points up or not */
  def points_up(a: Int, b: Int, c: Int): Boolean = a + b + c == 2


  /** Returns the array of the corners' coordinates in Cartersian coordinate system.
   *
   * @return [[List[Point]]]*/
  // TODO: add example in the method documentation
  // e.g. corner coordinates for tri(0,1,0), tri(1,1,0)
  def tri_corners(a: Int, b: Int, c: Int): List[(Double, Double)] = {
    if (points_up(a, b, c)) then
      List(tri_center(1 + a, b, c),
      tri_center(a, b, 1 + c),
      tri_center(a, 1 + b, c)
    )
    else List(
      tri_center(-1 + a, b, c),
      tri_center(a, b, -1 + c),
      tri_center(a, -1 + b, c)
    )
  }

  def pick_tri(x: Double, y: Double): (Double, Double, Double) = {
    // Using dot product, measures which row and diagonals a given point occupies.
    // Or equivalently, multiply by the inverse matrix to tri_center
    // Note we have to break symmetry, using floor(...)+1 instead of ceil, in order
    // to deal with corner vertices like (0, 0) correctly.
    (
      ceil((1 * x - sqrt3 / 3 * y) / edge_length),
      floor((sqrt3 * 2 / 3 * y) / edge_length) + 1,
      ceil((-1 * x - sqrt3 / 3 * y) / edge_length)
    )
  }

  def tri_neighbours(a: Double, b: Double, c: Double): List[(Double, Double, Double)] = {
    if (points_up(a, b, c)) List(
      (a - 1, b, c),
      (a, b - 1, c),
      (a, b, c - 1)
    )
    else List(
      (a + 1, b, c),
      (a, b + 1, c),
      (a, b, c + 1)
    )
  }

  def tri_dist(a1: Double, b1: Double, c1: Double, a2: Double, b2: Double, c2: Double): Double =
    abs(a1 - a2) + abs(b1 - b2) + abs(c1 - c2)

  def tri_disc(a: Double, b: Double, c: Double, r: Double): Seq[(Double, Double, Double)] = {
    // This could probably be optimized more
    (for (da <- -r to r) yield {
      (for (db <- -r to r) yield {
        val dc

end TriGridPos
package engine

import engine.grid.{GridPos, Point}
import engine.grid.TriGridPos
import math.sqrt
import engine.grid.grid.{coeff, edgeLength}


/** The class `TriHolder` represents the static slot (holder) that holds a
 *  triangle tile in a hexagon, as an abstract concept.
 *
 *  A `TriHolder` object is potentially mutable: its state can change as
 *  triangle tiles enter and exit it.
 */
class TriHolder(val pos: GridPos):

  private var occupant: Option[TriTile] = None
  private val triGridPos: TriGridPos = this.pos.toTriGridPos
  private val a: Int = this.triGridPos.a
  private val b: Int = this.triGridPos.b
  private val c: Int = this.triGridPos.c
  var corners: Vector[Point] = this._triCorners()


  /** Determines whether the holder points up or not
   *  (points up means one vertex is above, two other vertices are below)
   *  */
  def pointsUp: Boolean = this.pos.toTriGridPos.pointsUp


  /** Returns `true` if the holder has no robot in it, `false` otherwise. */
  def isEmpty = this.occupant == None


  /** Returns `true` if the holder contains any triangle tile, `false` otherwise. */
  def nonEmpty: Boolean = !this.isEmpty


  /** Returns the triangle tile occupying the holder, wrapped in an `Option`, or `None`,
   * if there is no tile in the holder. */
  def tile = this.occupant


  /** Removes and return any triangle tile from the holder (if there was one there to begin with). */
  def removeTile(): Option[TriTile] =
    val tile = this.occupant
    this.occupant = None
    tile

  
  /** Adds the given triangle tile in the holder, if possible. The arrival fails
   * if the holder already had an occupant. 
   *
   * (This method never affects the contents of any other holder besides the
   * single holder on which the method is invoked.)
   *
   * @param arrivee  the triangle tile attempting to arrive in the holder
   * @return `true` if `arrivee` was successfully placed in the holder, `false` otherwise */
  def addTile(arrivee: TriTile): Boolean =
    var successfullyAdded = false
    this.occupant match
      case None =>
        this.occupant = Some(arrivee)
        successfullyAdded = true
      case Some(tile) =>
        successfullyAdded = false
    successfullyAdded


  /** Returns the center (in cartesian co-ordinates) of triangle at position (a,b,c)
   * in triangular coordinate system.
   *
   * @example {{{
   * TriHolder(1,3).center() // i.e., (0,1,0) => Point(0, 0.57735) = (0, sqrt(3)/3)
   * }}}
   */
  def center: Point =
    Point((0.5 * this.a + -0.5 * this.c) * edgeLength,
      (-coeff / 6 * this.a + coeff / 3 * this.b - coeff / 6 * this.c) * edgeLength)


  /** Returns the array of the corners' coordinates in Cartersian coordinate system.
   * 
   * This method is borrowed from `TriTile` and uses some methods from `TriTile`
   * for calculation in triangular system.
   *
   * @example {{{
   * val triangle = TriHolder(1,3) // (0,1,0) in TriGridPos
   * triangle.triCorners = Vector(Point(-0.5, 0.8660254037844386), 
   *                              Point(0.5, 0.8660254037844386), 
   *                              Point(0.0, 0.0))
   * }}}
   * */
  private def _triCorners(): Vector[Point] =
    if this.pointsUp then
      val p1 = TriGridPos(this.a, this.b, 1+this.c).center // first corner
      val p2 = TriGridPos(this.a, 1+this.b, this.c).center // second corner
      val p3 = TriGridPos(1+this.a, this.b, this.c).center // third corner
      Vector[Point](p1, p2, p3)
    else
      val p1 = TriGridPos(-1+this.a, this.b, this.c).center // first corner
      val p2 = TriGridPos(this.a, this.b, -1+this.c).center // second corner
      val p3 = TriGridPos(this.a, -1+this.b, this.c).center // third corner
      Vector[Point](p1, p2, p3)


end TriHolder


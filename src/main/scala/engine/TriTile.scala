package engine

import scala.math.sqrt
import scala.annotation.targetName
import engine.grid._
import engine.grid.grid._

/** The class `TriTile` represents the triangle tile that can be added to,
 *  removed from, and rotated around in a hexagon.
 *
 *  Each `TriTile` can be added to a `TriHolder` in either a `GameBoard`
 *  or a `WaitingBoard`.
 * 
 *  Each `TriTile` has the following properties:
 *
 *     - `var a, b, c`: coordinate in triangular system
 *     - `var p1, p2, p3`: three corners of the triangle, always start from
 *       the rightmost corner and follow clockwise order
 *     - `var e1, e2, e3`: three edges correspond to corner pairs (p1-p2),
 *       (p2-p3), (p3-p1)
 * 
 *  The class `TriTile` is mutable, i.e., the coordinates a, b, c can be changed.
 *  These changes can be done with class's method `updateCoords()`.
 * 
 *  In addition, `TriTile` also has values attached to the edges of the triangle.
 *  When first creating a `TriTile` instance, the edges all have value -1.
 *  These values can be changed with method `updateEdgeValues()`.
 * 
 *  @see [[GameBoard.generateSolution()]]
 */
final case class TriTile(private var _a: Int, private var _b: Int, private var _c: Int):


  private var _owner: Option[Board] = None
  // public method for accessing owner
  def owner: Option[Board] = this._owner

  // public methods for accessing triangular coordinates
  def a: Int = this._a
  def b: Int = this._b
  def c: Int = this._c

  // calculate corners of triangle in 2D Cartersian system
  var corners: Vector[Point] = this._triCorners()
  private var p1: Point = this.corners(0)
  private var p2: Point = this.corners(1)
  private var p3: Point = this.corners(2)

  // initialize edges of triangle in 2D Cartersian system
  private var e1: Edge = Edge(this.p1, this.p2, -1)
  private var e2: Edge = Edge(this.p2, this.p3, -1)
  private var e3: Edge = Edge(this.p1, this.p3, -1)

  /** Return the list of edges of the triangle */
  def edges: Vector[Edge] = Vector(this.e1, this.e2, this.e3)


  /**
    * Return the position of TriTile in TriGridPos.
    */
  def triPos: TriGridPos = TriGridPos(this.a, this.b, this.c)


  /**
    * Return the position of TriTile in GridPos.
    */
  def pos: GridPos = this.triPos.toGridPos


  /** Returns the center (in cartesian co-ordinates) of triangle at position (a,b,c)
   * in triangular coordinate system.
   *
   * @example {{{
   * TriTile(0,1,0).center // Point(0, 0.57735) = (0, sqrt(3)/3)
   * TriTile(1,1,0).center // Point(0.5, 0.288675) = (1/2, sqrt(3)/6)
   * TriTile(1,0,0).center // Point(0.5, -0.288675) = (1/2, -sqrt(3)/6)
   * }}}
   */
  def center: Point =
    this.triPos.center


  /** Determines whether the triangle points up or not */
  def pointsUp: Boolean = this.triPos.pointsUp


  /** Returns the array of the corners' coordinates in Cartersian coordinate system.
   *
   * @example {{{
   * val triangle = TriTile(0,1,0)
   * triangle._triCorners() = Vector(Point(-0.5, 0.8660254037844386), 
   *                                 Point(0.5, 0.8660254037844386), 
   *                                 Point(0.0, 0.0))
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


  /** Return the values attached to the edges of the triangle. */
  def values: Vector[Int] = this.edges.map(_.value)


  /**
    * Update the owner of this `TriTile`.
    * 
    * @param newOwner
    */
  def updateOwner(newOwner: Board) =
    this._owner = Some(newOwner)


  /**
    * Remove the owner of this `TriTile`.
    */
  def removeOwner() =
    this._owner = None


  /**
    * Update the value of an edge of the `TriTile`.
    *
    * @param edge
    * @param newVal
    * @return Boolean value indicating whether the update succeeded or not.
    */
  def updateEdge(edge: Edge, newVal: Int): Boolean =
    if this.edges.contains(edge) then
      this.edges.indexOf(edge) match
        case 0 => this.e1 = Edge(this.p1, this.p2, newVal)
        case 1 => this.e2 = Edge(this.p2, this.p3, newVal)
        case 2 => this.e3 = Edge(this.p1, this.p3, newVal)
        true
    else
      false


  /**
    * Update the values of the edges of the triangle.
    *
    * @param val1 new value of first edge (i.e., e1 connecting p1 and p2)
    * @param val2 new value of second edge (i.e., e2 connecting p2 and p3)
    * @param val3 new value of third edge (i.e., e3 connecting p1 and p3)
    */
  def updateEdgeValues(val1: Int, val2: Int, val3: Int) =
    val newEdge1: Edge = Edge(this.p1, this.p2, val1)
    val newEdge2: Edge = Edge(this.p2, this.p3, val2)
    val newEdge3: Edge = Edge(this.p1, this.p3, val3)
    this.e1 = newEdge1
    this.e2 = newEdge2
    this.e3 = newEdge3


  /**
    * Update new coordinates of the `TriTile` instance.
    * 
    * In addition to `a, b, c` the instance variables `p1, p2, p3`
    * and `e1, e2, e3` will also be updated according to the new
    * triangular coordinates.
    */
  def updateCoords(a: Int, b: Int, c: Int) = 
    // update instance coordinate in triangular system
    this._a = a
    this._b = b
    this._c = c
    // update coordinates of corners
    this.corners = this._triCorners()
    this.p1 = this.corners(0)
    this.p2 = this.corners(1)
    this.p3 = this.corners(2)
    // update the coordinates of the edges
    this.e1 = Edge(this.p1, this.p2, this.e1.value)
    this.e2 = Edge(this.p2, this.p3, this.e2.value)
    this.e3 = Edge(this.p1, this.p3, this.e3.value)


  /** 
   * Returns the list of this triangle's neighbors on the game board.
   * 
   * It uses method `neighbors()` from [[TriGridPos]] to find the neighbor
   * locations with respect to the location of this `TriTile`. Then it
   * will retrieve all the `TriTile`s at that location of the board.
   * 
   * 
   * @example {{{
   * TriTile(0,1,1).neighbors = 
   *   Vector(TriTile(-1,1,1), TriTile(0,0,1), TriTile(0,1,0)) // full 3 neighbors
   * TriTile(0,1,0).neighbors = 
   *   Vector(TriTile(1,1,0), TriTile(0,2,0)) // has only 2 neighbors
   * }}}
   * */
  def neighbors: Vector[TriTile] =
    val neighborLoc = this.triPos.neighbors
    val neighborInGridPos = neighborLoc.map(pos => triGridToGrid(pos.a, pos.b, pos.c)) // conver (a,b,c) to (x,y)
                                       .map((x,y) => GridPos(x,y)) // get the GridPos objects
    val neighborTileOption = neighborInGridPos.map(pos => this.owner.get.elementAt(pos).tile)
    neighborTileOption.collect{
      case Some(tile) => tile
    }.toVector


  /** 
   * Returns the neighbor of this `TriTile` given an edge wrapped in an `Option`.
   * If there is no neighbor with respect to that edge of the `TriTile`, the
   * return value is `None`.
   * */
  def neighbor(edge: Edge): Option[TriTile] =
    neighbors.find(_.edges.exists(_.matchingCoordinates(edge))) match
      case Some(neighbor) => Some(neighbor)
      case None => None


  /**
    * Return the edge that is adjacent to the given edge of this `TriTile`.
    * If there is no neighbor with that edge of the `TriTile`, the adjacent
    * edge value is `None`.
    */
  def adjacentEdge(edge: Edge): Option[Edge] =
    val triTileOpt = this.neighbor(edge)
    triTileOpt.flatMap(triTile =>
      triTile.edges.find(_.matchingCoordinates(edge)))


  /**
    * Rotate the `TriTile` 120 degrees clockwise.
    * 
    * When the triangle performs this rotation, the triangle coordinates
    * (i.e., `a, b, c`), corners (i.e., `p1, p2, p3`) remain the same. 
    * What changes is the values of the edges of the triangle.
    * 
    * For example, `TriTile(0,1,0)` (points down) at `GridPos(1,3)` 
    * (same as `TriGridPos(0,1,0)`) has:
    *
    * {{{
    * e1 = Edge(p1, p2, 1)
    * e2 = Edge(p2, p3, 2)
    * e3 = Edge(p1, p3, 3)
    * }}}
    * 
    * After performing `rotateClockwise()`, the edges are:

    * {{{
    * e1 = Edge(p1, p2, 3)
    * e2 = Edge(p2, p3, 1)
    * e3 = Edge(p1, p3, 2)
    * }}}
    * 
    * Another example, `TriTile(1,1,0)` (points up) at `GridPos(1,4)` 
    * (same as `TriGridPos(1,1,0)`) has:
    *
    * {{{
    * e1 = Edge(p1, p2, 1)
    * e2 = Edge(p2, p3, 2)
    * e3 = Edge(p1, p3, 3)
    * }}}
    * 
    * After performing `rotateClockwise()`, the edges are:

    * {{{
    * e1 = Edge(p1, p2, 3)
    * e2 = Edge(p2, p3, 1)
    * e3 = Edge(p1, p3, 2)
    * }}}
    */
  def rotateClockwise() = 
    val newValEdge1 = this.e3.value
    val newValEdge2 = this.e1.value
    val newValEdge3 = this.e2.value
    this.updateEdgeValues(newValEdge1, newValEdge2, newValEdge3)


  /**
    * Rotate the `TriTile` 120 degrees counter-clockwise.
    * 
    * Similar to method `rotateClockwise()`, when the triangle 
    * performs this rotation, the triangle coordinates
    * (i.e., `a, b, c`), corners (i.e., `p1, p2, p3`) remain the same. 
    * What changes is the values of the edges of the triangle.
    * 
    * For example, `TriTile(0,1,0)` (points down) at `GridPos(1,3)` 
    * (same as `TriGridPos(0,1,0)`) has:
    *
    * {{{
    * e1 = Edge(p1, p2, 1)
    * e2 = Edge(p2, p3, 2)
    * e3 = Edge(p1, p3, 3)
    * }}}
    * 
    * After performing `rotateClockwise()`, the edges are:

    * {{{
    * e1 = Edge(p1, p2, 2)
    * e2 = Edge(p2, p3, 3)
    * e3 = Edge(p1, p3, 1)
    * }}}
    */
  def rotateCounterClockwise() = 
    val newValEdge1 = this.e2.value
    val newValEdge2 = this.e3.value
    val newValEdge3 = this.e1.value
    this.updateEdgeValues(newValEdge1, newValEdge2, newValEdge3)


  /**
    * Flip the `TriTile` from pointing up to point down and vice versa.
    * 
    * In the hexagonal board, each holder either points up or down. Therefore,
    * a `TriTile` cannot be simply flipped as it will not fit in its current
    * holder after the flip.
    * 
    * Instead, after a flip, pointing up `TriTile` will point down and vice versa.
    * Then the tile will be transfered to the first empty holder for its new pointing
    * direction.
    * 
    * For example, `TriTile(0,1,0)` points down, and after `TriTile(0,1,0).flipTri()`,
    * it will be transferred to the first empty holder that points up
    * (e.g., TriGridPos(-1,2,1) or TriGridPos(0,2,0)).
    * 
    * @return Boolean value indicating whether the process succeeded or not.
    */
  def flipTri(): Boolean = 
    val emptyHolders = this.pointsUp match // get list of empty TriHolder
      case true => this.owner.get.downHolders.filter(_.isEmpty)
      case false => this.owner.get.upHolders.filter(_.isEmpty)
    if emptyHolders.length == 0 then // cannot fit in other holder of the board
      false
    else
      val newPos = emptyHolders(0).pos // get the first empty position
      val oldPos = this.pos
      val owner = this.owner.get
      owner.moveTile(owner, oldPos, newPos)


  /**
    * Determine if this `TriTile` is identical to another `TriTile`.
    * 
    * Two `TriTile`s are identical when their edges have the same values
    * (regardless of the edge values orders).
    *
    * @example {{{
    * val x = TriTile
    * x.values = Vector(1,2,3)
    * 
    * val y = TriTile
    * y.values = Vector(3,1,2)
    * 
    * x == y // true
    * }}}
    * 
    * @param another another `TriTile`
    * @return Boolean value
    */
  @targetName("equals")
  def ==(another: TriTile): Boolean =
    val v1 = this.values
    val v2 = another.values
    if (v1.length != v2.length) then
      false
    else
      val v = v1 ++ v1
      val index = v.indexOfSlice(v2)
      index >= 0


end TriTile


package engine.grid

import engine.grid.grid.{gridToTriGrid, triGridToGrid}
import scala.annotation.targetName

/** An object of type `GridPos` represents a pair of integer coordinates. Such a pair can
 * be used to reference a position on a [[TriGrid]].
 *
 * If the coordinates of `GridPos` are valid, it will have a compatible `TriGridPos` object
 * which refers to the same position on the [[TriGrid]]
 *
 * The coordinate axes are named `x` and `y`. In this coordinate system, `x` increases
 * “eastwards” and y` increases “southwards”.
 *
 * `GridPos` objects are immutable.
 *
 * @param x  an x coordinate
 * @param y  a y coordinate */
final case class GridPos(val x: Int, val y: Int):

  /** Returns the compatible TriGridPos coordinate on [[TriGrid]].
   *  The method here assumes that the GridPos coordinates are valid.
   *  */
  def toTriGridPos: TriGridPos =
    val (a, b, c) = gridToTriGrid((this.x, this.y))
    TriGridPos(a, b, c)


  /** Determines whether this grid position equals the given one. This is the case if
   * the two have identical x and y coordinates. */
  @targetName("equals")
  def ==(another: GridPos): Boolean =
    this.x == another.x &&
    this.y == another.y


  /** Returns the vector of this triangle's neighbors.
   *
   *  The neighbors of a `GridPos` are calculated by first converting `GridPos` to its
   *  compatible `TriGridPos`, then find all the neighbors with respect to that
   *  `TriGridPos`, and return the Seq of compatible `GridPos`
   *  */
  def neighbors: Seq[GridPos] =
    this.toTriGridPos.neighbors.map(neigbor => neigbor.toGridPos)

end GridPos



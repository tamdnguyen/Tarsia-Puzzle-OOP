package engine

import engine.Wall.robot
import engine.grid.{GridPos, Point}

import scala.math.sqrt

/** The class `TriHolder` represents the static slot (holder) that holds a
 *  triangle tile in a hexagon, as an abstract concept.
 *
 *  A `TriHolder` object is potentially mutable: its state can change as
 *  triangle tiles enter and exit it.
 */
class TriHolder(val pos: GridPos):

  private var occupant: Option[TriTile] = None

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
  def removeTile(): TriTile =
    this.occupant = None

  /** Adds the given robot in the floor square, if possible. The arrival fails
   * if the square already had an occupant. In that case, the occupant gets
   * destroyed; the robot that crashed into it stays intact and does not get
   * added in the square.
   *
   * (This method never affects the contents of any other square besides the
   * single floor square on which the method is invoked.)
   *
   * @param arrivee  the robot attempting to arrive in the square
   * @return `true` if `arrivee` was successfully placed in the square, `false` if a collision occurred */
  // TODO: rewrite comment
  def addTile(arrivee: TriTile): Boolean =
    var successfullyAdded = false
    this.occupant match
      case None =>
        this.occupant = Some(arrivee)
        successfullyAdded = true
      case Some(tile) =>
        successfullyAdded = false
    successfullyAdded


end TriHolder

package engine

import engine.grid.{GridPos, Point}

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


end TriHolder

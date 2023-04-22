package engine

import engine.grid.{Point, GridPos, TriGrid}
import engine.grid.grid._
import math.{ceil, floor}
import scala.collection.mutable.ArrayBuffer
import engine.grid.TriGridPos

/** An instance of the class `Board` represents a two-dimensional hexagon
 * that can be inhabited by triangle tiles. This kind of "Board" is a `TriGrid`
 * whose elements are `TriHolder` objects.
 *
 * Triangle tile — `TriTile` objects — can be added to the board, and the board
 * object maintains a listing of the added tiles.
 *
 * In the scope of this project, the instances of the Board can only have
 * `width=7` and `height=4`. The constraints come from the constant hashmap that
 * translate between `GridPos` and `TriGridPos`. This issue must be improved
 * with better implementation rather than a constant hashmap
 * if we want a scalable project.
 *
 * @param width   the width (max width) of the board, in holders.
 * @param height  the height (max height) of the board, in holders.
 * @see [[TriGrid]] */
class Board(width: Int, height: Int) extends TriGrid[TriHolder](width, height):

  private val tiles = ArrayBuffer[TriTile]()


  /** Generates the elements that initially occupy the grid. In the case of a `Board` grid, an
   * element is a `TriHolder` object.
   *
   * This method is automatically invoked by the superclass `TriGrid` to initialize the grid’s contents.
   *
   * @return a collection that contains the initial grid elements. The first element will
   *         appear at `GridPos` (0,0), the second at (1,0), and so on, filling in the first
   *         row before continuing on the second row at (0,1). */
  def initialElements = 
    for i <- 0 until this.allPositions.length yield initializeHolder(this.allPositions(i))


  /** Returns the `TriHolder` that should initially appear at the given coordinates within in a newly created
   * `Board`.*/
  private def initializeHolder(pos: GridPos) =
    TriHolder(pos)


  /** Creates a new tile into this board.
   *
   * This method is responsible for several related things: 
   * 
   *  - creating the tile
   *  - updating the tile's owner
   *  - adding the tile to the list of tiles in this board (so it can be act in a turn)
   *  - informing the tile's initial holder that the tile is now there (by calling the
   *    holder's `addTile` method).
   *
   * In case the position is already occupied, it simply returns the tile that occupies the holder.
   * 
   * @param initialLocation  the initial location of the new tile in this board. This method assumes that `location` points to an empty holder.
   * @return the newly created tile if empty position, or the tile that occupies the position
   * */
  def initializeTile(initialLocation: GridPos): TriTile =
    if this(initialLocation).nonEmpty then
      this(initialLocation).tile.get
    else
      val locTriGrid = initialLocation.toTriGridPos
      val newTile = TriTile(locTriGrid.a, locTriGrid.b, locTriGrid.c) // create new tile
      newTile.updateOwner(this) // updating the tile's owner
      val len = this.tiles.length
      newTile.updateEdgeValues(-(10*len + 1), -(10*len+2), -(10*len+3)) // assign unique value for new edges
      this.tiles += newTile // add tile to the list
      this.elementAt(initialLocation).addTile(newTile) // adding tile to the holder
      newTile


  /** Add an existing tile into this board.
   *
   * This method is responsible for several related things: 
   *
   *   - adding the tile to the list of tiles in this board (so it can be act in a turn)
   *   - updating the tile's owner
   *   - updating the new coordinates to the tile
   *   - informing the tile's initial holder that the tile is now there (by calling the
   *     holder's `addTile` method).
   *
   * @param location  the location to add the tile in this board. This method assumes that `location` points to an empty holder.*/
  def addTile(tile: TriTile, location: GridPos) =
    this.tiles += tile // add tile to the list
    tile.updateOwner(this) // updating the tile's owner
    val newPos = location.toTriGridPos
    tile.updateCoords(newPos.a, newPos.b, newPos.c) // updating the new coordinates of the tile
    this.elementAt(location).addTile(tile) // adding tile to the holder


  /** Remove the tile at the given position of the board.
   *
   * This method is responsible for several related things: 
   * 
   *   - removing the tile from the list of tiles in this board
   *   - setting tile's owner to None
   *   - updating invalid coordinates to the removed tile
   *   - removing the tile from its holder
   * 
   * After being removed, the tile will have coordinates (0,0,0), which is an
   * invalid coordinates in the grid system. This behavior ensures that the
   * usage of `removeTile()` always have a corresponding `addTile()`. The calling
   * of both `addTile()` and `removeTile()` is performed in method `exchangeTile()`.
   * 
   * @see [[Board.moveTile()]]
   * @see [[Board.exchangeTile()]]
   *
   * @param location  the location to remove the tile in this board. This method assumes that `location` points to an non-empty holder.*/
  def removeTile(location: GridPos): TriTile =
    val tileRemoved = this.elementAt(location).removeTile().get // removing tile from the holder
    this.tiles -=  tileRemoved // remove tile from the list
    tileRemoved.removeOwner() // remove the tile's owner
    tileRemoved.updateCoords(0,0,0) // update invalid coordinates
    tileRemoved


  /** Move a tile from this board to another board in case the exchange
   *  process can be performed.
   *  In the context of this game, this exchange is from [[GameBoard]] to
   *  [[WaitingBoard]] or vice versa, or from a board to itself.
   * 
   *  This method is responsible for checking the validity of the holders
   *  (i.e., the `posFrom` location's holder does contain a triangle tile,
   *  the `posTo` location's holder is empty).
   * 
   *  The move is only possible when `posFrom` is non-empty. If `posTo` is
   *  empty, the method simply moves the tile to the new position. If
   *  `posTo` is non-empty, the method will exchange the tiles at the locations.
   *  
   * 
   *  @param another  the board that the tile is exchanged to.
   *  @param posFrom  the `GridPos` location in this board that the tile is removed.
   *  @param posTo    the `GridPos` location in the other board that the tile will be moved to.
   *  @return Boolean value indicating whether the exchange process is able or not.
   * */
  def moveTile(another: Board, posFrom: GridPos, posTo: GridPos): Boolean =
    // special case when the move is from a gridpos to that same gridpos on a same board
    if this.eq(another) && posFrom.equals(posTo) then
      true
    else
      val posFromIsNonEmpty: Boolean = this.elementAt(posFrom).nonEmpty
      val posToIsEmpty: Boolean = another.elementAt(posTo).isEmpty

      if posFromIsNonEmpty && posToIsEmpty then
        val tile = this.removeTile(posFrom)
        another.addTile(tile, posTo)
      else if posFromIsNonEmpty && !posToIsEmpty then
        this.exchangeTile(another, posFrom, posTo)
      else
        false
  

  /** Swap a tile from this board with a tile from another board. 
   *  In the context of this game, this exchange is from [[GameBoard]] to
   *  [[WaitingBoard]] or vice versa, or from a board to itself.
   * 
   *  The swap is only possible when both positions are non-empty.
   * 
   *  @param that     the board that the tile is exchanged to.
   *  @param posThis  the `GridPos` location in this board.
   *  @param posThat  the `GridPos` location in the other board.
   *  @return Boolean value indicating whether the exchange process is able or not.
   * */
  def exchangeTile(that: Board, posThis: GridPos, posThat: GridPos): Boolean =
    val thisEmpty: Boolean = this.elementAt(posThis).isEmpty
    val thatEmpty: Boolean = that.elementAt(posThat).isEmpty

    if !thisEmpty && !thatEmpty then
      val tileOfThis = this.removeTile(posThis)
      val tileofThat = that.removeTile(posThat)
      this.addTile(tileofThat, posThis)
      that.addTile(tileOfThis, posThat)
      true
    else
      false


  /**
    * Given a position in cartersian coordinate, return the GridPos and the TriTile
    * at that position, both wrapped in Option.
    *
    * @param pos coordinate in cartesian
    * @return Tuple (Option[GridPos], Option[TriTile]). 
    */
  def pickTile(pos: Point): (Option[GridPos], Option[TriTile]) =
    val x = pos.x
    val y = pos.y
    val a = ceil(( 1 * x - coeff / 3 * y) / edgeLength).toInt
    val b = floor(( coeff * 2 / 3 * y) / edgeLength).toInt + 1
    val c = ceil((-1 * x - coeff / 3 * y) / edgeLength).toInt
    val tile: Option[TriTile] = this.tileList.find(tile => tile.a == a && tile.b == b && tile.c == c) 
    val gridPos: Option[GridPos] = triGridToGrid.get((a,b,c)) match
      case Some(value) => Some(GridPos(value._1, value._2))
      case None => None
    (gridPos, tile)


  /** Returns the number of tile that have been added to this board. */
  def numberOfTiles: Int = this.tiles.size


  /** Returns a collection of all the tiles in this board, in the order they were added to the board. */
  def tileList = this.tiles


  /**
    * Return the list of all `TriHolder`'s that points up.
    */
  def upHolders: Seq[TriHolder] = this.allElements.filter(_.pointsUp)


  /**
    * Return the list of all `TriHolder`'s that points down.
    */
  def downHolders: Seq[TriHolder] = this.allElements.filter(!_.pointsUp)


end Board



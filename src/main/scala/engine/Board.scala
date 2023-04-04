package engine

import engine.grid.{GridPos, TriGrid}

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

  private var tiles = Vector[TriTile]()


  /** Generates the elements that initially occupy the grid. In the case of a `Board` grid, an
   * element is a `TriHolder` object.
   *
   * This method is automatically invoked by the superclass `TriGrid` to initialize the grid’s contents.
   *
   * @return a collection that contains the initial grid elements. The first element will
   *         appear at `GridPos` (0,0), the second at (1,0), and so on, filling in the first
   *         row before continuing on the second row at (0,1). */
  def initialHolders = for y <- 0 until this.height; x <- 0 until this.width yield initializeHolder(x, y)


  /** Returns the `TriHolder` that should initially appear at the given coordinates within in a newly created
   * `Board`.*/
  private def initializeHolder(x: Int, y: Int) =
    TriHolder(GridPos(x,y))


  /** Creates a new tile into this board.
   *
   * This method is responsible for several related things: creating the tile,
   * adding the tile to the list of tiles in this board (so it can be act in a turn),
   * and informing the tile's initial holder that the tile is now there (by calling the
   * holder's `addTile` method).
   *
   * @param initialLocation  the initial location of the new tile in this board. This method assumes that `location` points to an empty holder.
   * @return the newly created tile, which has been placed in the indicated holder */
  def initializeTile(initialLocation: GridPos): TriTile =
    val locTriGrid = initialLocation.toTriGridPos
    val newTile = TriTile(locTriGrid.a, locTriGrid.b, locTriGrid.c) // create new tile
    this.tiles = this.tiles :+ newTile // add tile to the list
    this.elementAt(initialLocation).addTile(newTile) // adding tile to the holder
    newTile


  //TODO
  def addTile(tile: TriTile, location: GridPos) = ???

  //TODO
  def removeTile(location: GridPos) = ???

  /** Returns the number of tile that have been added to this board. */
  def numberOfTiles: Int = this.tiles.size


  /** Returns a collection of all the tiles in this board, in the order they were added to the board. */
  def tileList = this.tiles


end Board

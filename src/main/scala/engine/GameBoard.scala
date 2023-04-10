package engine


/** A `GameBoard` is an extension of the [[Board]] object. This means that
 * a `GameBoard` is also a board that is inhabited by triangle tiles.
 * 
 * Besides the basic functionality of holding the `TriHolder` and the `TriTile`
 * objects, a `GameBoard` also contains some important methods that implement
 * the logic of the game.
 * 
 * For example, `generateSolution()` to generate a valid solution of a game,
 * and `shuffleTiles()` to shuffle the tiles and start the game. 
 * 
 * In the scope of this project, the instances of the `GameBoard` can only have
 * `width=7` and `height=4`. This constraint is explained well in [[Board]] and
 * [[TriGrid]].
 * 
 * @see [[TriGrid]] 
 * @see [[Board]] 
 * @see [[engine.grid.package]] 
 * */
class GameBoard extends Board(7, 4):

  /**
    * Determines whether the game board is filled with 
    * 24 `TriTile`s or not.
    *
    * @return Boolean value
    */
  def isFilled: Boolean = this.numberOfTiles == 24


  /**
    * Determine if the current state of the game board is a valid solution.
    * A valid solution is a configuration of the game board which has 24 
    * `TriTile`s and all the adjacent edges share the same value. 
    * In addition, a valid solution also must not have any two
    * identical triangles (same values of the edges regardless of the edge order).
    *
    * Algorithm: iterate through the rows, tile by tile and check with the condition
    * 
    * @return Boolean value
    */
  def isCompleted: Boolean = ???


  /**
    * Generates a valid solution of a game. A valid solution is a configuration
    * of the game board which has 24 `TriTile`s and all the adjacent edges share
    * the same value. In addition, a valid solution also must not have any two
    * identical triangles (same values of the edges regardless of the edge order).
    * 
    * Algorithm: 
    *   
    *   - Start from the top left-most tile in the board (`GridPos(0,0)` or `TriGridPos(-1,2,1)`)
    *   - Randomly generate the values for the edges
    *   - Move on to the next tile on that row (i.e., `GridPos(0,1)`)
    *   - Generate the values on the edges of that tile such that the adjacent edges 
    *     between (0,0) and (0,1) share the same values.
    *   - Repeat the process until all 24 `TriTile`s are filled on the board.
    */
  def generateSolution() = ???


  /**
    * Shuffles the tiles of a valid solution to start a new game.
    * 
    * Algorithm: 
    * 
    *   - randomly shuffle the array of pointy triangles 
    *   - randomly choose some tiles from pointy array and perform rotation on them
    *   - place them back to the hexagon
    *   - repeat the process with the array of flat triangles
    */
  def shuffleTiles() = ???


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

  def isFilled: Boolean = ???

  def isCompleted: Boolean = ???

  def generateSolution() = ???

  def shuffleTiles() = ???


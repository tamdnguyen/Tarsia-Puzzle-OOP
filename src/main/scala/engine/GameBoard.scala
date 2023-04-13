package engine

import engine.grid.grid._
import scala.util.Random

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
  def isFilled: Boolean = this.numberOfTiles == this.size


  /**
    * Determine if all adjacent edges of the current board share 
    * the same value pair or not.
    */
  def allMatchingEdges: Boolean = 
    // Iterate over each TriTile on the board
    this.tileList.forall( tile =>
      // Iterate over each Edge of the TriTile
      tile.edges.forall( edge =>
        // Check if there is an adjacent edge
        tile.adjacentEdge(edge) match 
          // If there is no adjacent edge, return true
          case None => true
          // If there is an adjacent edge, check if their values match
          case Some(adjacentEdge) => edge.matchingValue(adjacentEdge)
      )
    )


  /**
    * Determine if the current board has two
    * identical triangles or not.
    * 
    * Two identical triangles mean they share the same values 
    * of the edges regardless of the edge order.
    */
  def hasIdenticalTiles: Boolean = 
    this.tileList.combinations(2).exists(tiles => tiles(0) == tiles(1))


  /**
    * Determine if the current state of the game board is a valid solution.
    * A valid solution is a configuration of the game board which has 24 
    * `TriTile`s and all the adjacent edges share the same value. 
    * In addition, a valid solution also must not have any two
    * identical triangles (same values of the edges regardless of the edge order).
    * 
    * In other word, `isCompleted = isFilled && allMatchingEdges && noIdenticalTiles`.
    */
  def isCompleted: Boolean = 
    this.isFilled && this.allMatchingEdges && !this.hasIdenticalTiles


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
  def generateSolution() = 
    this.allPositions.foreach(this.initializeTile(_)) // initialize all tiles with edge value -1
    this.tileList(0).updateEdgeValues(this.randEdgeVal(), // random value for 3 edges of first TriTile
                                      this.randEdgeVal(), 
                                      this.randEdgeVal())

    // for i <- 1 until this.numberOfTiles do
    //   val currTile = this.tileList(i)
    //   currTile.edges.foreach( edge => 
    //     currTile.adjacentEdge(edge) match
    //       case Some(otherEdge) => 
    //         // update edge value such that
    //         // it does not create identical TriTiles
    //         currTile.updateEdge(edge, edge.matchEdgeVal(otherEdge.value))
    //         while this.hasIdenticalTiles do
    //           currTile.updateEdge(edge, edge.matchEdgeVal(otherEdge.value))
    //       case None => 
    //         currTile.updateEdge(edge, this.randEdgeVal())
    //         // choose some random value for the edge such that
    //         // it does not create identical TriTiles
    //         while this.hasIdenticalTiles do
    //           currTile.updateEdge(edge, this.randEdgeVal())
    //   )
    for i <- 1 until this.numberOfTiles do
      val currTile = this.tileList(i) 

      // first update of the edge values of tile(i)
      for j <- 0 until currTile.edges.length do
        val edge = currTile.edges(j)
        currTile.adjacentEdge(edge) match
          case Some(otherEdge) => 
            currTile.updateEdge(edge, edge.matchEdgeVal(otherEdge.value))
          case None => 
            currTile.updateEdge(edge, this.randEdgeVal())
      
      // the update may create identical tiles on the board
      // choose an unrestricted edge 
      val edgeToChange = currTile.edges.find( edge =>
        currTile.adjacentEdge(edge) match
          case Some(otherEdge) => !edgeValues.contains(otherEdge.value)
          case None => true
      ).get
      // and keep updating value until no more identical
      while this.hasIdenticalTiles do
        currTile.updateEdge(edgeToChange, this.randEdgeVal())

    require(this.tileList.map(_.values).forall(_.forall(_ > 0)), 
            s"Configuration after generateSolution must only have positive edge values.")
    // require(this.isCompleted, 
    //         s"Configuration after generateSolution must be a valid solution.")


  /**
    * Shuffles the tiles of a valid solution to start a new game.
    * 
    * Algorithm: 
    * 
    *   - randomly choose two positions from the board
    *   - exchange the tiles of those position
    *   - repeat this.size times
    *   - randomly choose a tile from the board
    *   - rotate it clockwise
    *   - repeat this.size times
    * 
    * Shuffle action should only happen once after generateSolution().
    * In other words, the board can only be shuffled when it is having the valid solution.
    */
  def shuffleTiles() = 
    if this.isCompleted then
      // Repeat the swapping 24 times
      for (_ <- 0 until this.size)
        // Select two random GridPos from allPositions
        val pos1 = this.allPositions(Random.nextInt(this.allPositions.length))
        val pos2 = this.allPositions(Random.nextInt(this.allPositions.length))
        // Call exchangeTile function on the two positions
        if !(pos1 == pos2) then
          this.exchangeTile(this, pos1, pos2)
      // Repeat the rotating 24 times
      for (_ <- 0 until this.size)
        // Select a random tile from the tileList
        val tile = this.tileList(Random.nextInt(this.tileList.length))
        // rotate the tile
        tile.rotateClockwise()


  /**
    * Returns a random value from the list of possible edge values.
    */
  def randEdgeVal(): Int = 
    val randomIndex = Random.nextInt(edgeValues.length) 
    edgeValues(randomIndex)


end GameBoard
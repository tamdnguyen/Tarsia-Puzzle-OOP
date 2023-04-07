package engine

/** A `WaitingBoard` is an extension of the [[Board]] object. This means that
 * a `WaitingBoard` is also a board that is inhabited by triangle tiles.
 * 
 * A `WaitingBoard` is simply a containers for the `TriHolder` and `TriTile` objects.
 * It serves as a "free-to-use" zone with the main purpose of supporting the players.
 * It is where the players can put some tiles on to simplify their game.
 * 
 * In the scope of this project, the instances of the `WaitingBoard` can only have
 * `width=7` and `height=4`. This constraint is explained well in [[Board]] and
 * [[TriGrid]].
 * 
 * @see [[TriGrid]] 
 * @see [[Board]] 
 * @see [[engine.grid.package]] 
 * */
class WaitingBoard extends Board(7, 4)
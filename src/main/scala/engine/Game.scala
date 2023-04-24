package engine

import engine.grid.grid._
import engine.grid.GridPos
import java.io.{File, PrintWriter}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{write, writePretty}
import io.circe._
import io.circe.parser._
import io.circe.generic.semiauto._


class Game:
  var gameBoard: GameBoard = new GameBoard()
  var waitingBoard: WaitingBoard = new WaitingBoard()
  private var moveCount: Int = 0
  

  /**
    * Generate a new GameBoard along with a new WaitingBoard.
    * 
    * GameBoard will generate a valid solution and then shuffle
    * the tiles.
    */
  def newGame(): Unit =
    this.gameBoard = new GameBoard()
    this.waitingBoard = new WaitingBoard()
    this.gameBoard.generateSolution()
    this.gameBoard.shuffleTiles()
  

  /**
    * Save the current game status to a JSON file.
    * JSON object:
    *   - moveCount
    *   - gameBoard
    *   - waitingBoard
    *
    * @param filename
    */
  def saveGame(filename: String): Unit = 
    implicit val formats: DefaultFormats.type = DefaultFormats
    val gameBoardTiles = gameBoard.tileList.map(t => Map(
      "a" -> t.a,
      "b" -> t.b,
      "c" -> t.c,
      "edges" -> t.values
    ))
    val waitingBoardTiles = waitingBoard.tileList.map(t => Map(
      "a" -> t.a,
      "b" -> t.b,
      "c" -> t.c,
      "edges" -> t.values
    ))
    val gameData = Map(
      "moveCount" -> this.moveCount,
      "gameBoard" -> gameBoardTiles,
      "waitingBoard" -> waitingBoardTiles
    )
    val jsonString = writePretty(gameData)
    val file = new File(filename)
    val writer = new PrintWriter(file)
    writer.write(jsonString)
    writer.close()


  /**
    * Load a game from a JSON file.
    * JSON object:
    *   - moveCount
    *   - gameBoard
    *   - waitingBoard
    *
    * @param filename
    */
  def loadGame(filename: String): (Boolean, String) = 
    val gameBoard = new GameBoard()
    val waitingBoard = new WaitingBoard()
    var moveCount: Int = -1

    // Load JSON data from file
    val file = new File(filename)
    if (!file.exists() || !file.isFile || !filename.endsWith(".json")) then
      return (false, s"Invalid file: $filename")

    val json = scala.io.Source.fromFile(file).mkString

    implicit val tileDecoder: Decoder[TriTileJSON] = deriveDecoder[TriTileJSON]
    implicit val gameDecoder: Decoder[GameDataJSON] = deriveDecoder[GameDataJSON]

    val decoded = decode[GameDataJSON](json)

    decoded match
      case Right(gameData) =>
        try // try to assign the game move count
          if gameData.moveCount < 0 then
            throw new RuntimeException("Invalid move count")
          moveCount = gameData.moveCount
        catch
          case e: Exception => return (false, s"Invalid data for move count: ${e.getMessage()}")
        try // try to update the tiles of the game board
          gameData.gameBoard.foreach( tileData =>
            val a: Int = tileData.a 
            val b: Int = tileData.b
            val c: Int = tileData.c 
            val edges: List[Int] = tileData.edges
            if !edges.forall(edgeValues.contains(_)) then
              throw new RuntimeException("Invalid edge color")
            val tile = new TriTile(a,b,c)
            tile.updateEdgeValues(edges(0), edges(1), edges(2))
            gameBoard.addTile(tile, tile.pos)
          )
        catch
          case e: Exception => return (false, s"Invalid data for game board: ${e.getMessage()}")
        try // try to update the tiles of the waiting board
          gameData.waitingBoard.foreach( tileData =>
            val a: Int = tileData.a 
            val b: Int = tileData.b
            val c: Int = tileData.c 
            val edges: List[Int] = tileData.edges
            if !edges.forall(edgeValues.contains(_)) then
              throw new RuntimeException("Invalid edge color")
            val tile = new TriTile(a,b,c)
            tile.updateEdgeValues(edges(0), edges(1), edges(2))
            waitingBoard.addTile(tile, tile.pos)
          )
        catch
          case e: Exception => return (false, s"Invalid data for waiting board: ${e.getMessage()}")
      case _ => return (false, "Invalid game data")

    // update the game to the loaded game
    this.moveCount = moveCount
    this.gameBoard = gameBoard
    this.waitingBoard = waitingBoard

    (true, "Game loaded successfully!")

  
  /**
    * Advancing the game and increase the moveCount.
    */
  def advance() = 
    this.moveCount += 1


  /**
    * Return the status of the game.
    * 
    * Move count if the game is not completed yet.
    * Otherwise, a congratulation text.
    */
  def status(): String =
    if this.gameBoard.isCompleted then
      "Congratulations! You have won the game!"
    else
      s"Move ${this.moveCount}"

end Game
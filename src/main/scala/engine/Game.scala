package engine

import engine.grid.GridPos
import java.io.{File, PrintWriter}
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{write, writePretty}


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
      "a" -> t.a.toString,
      "b" -> t.b.toString,
      "c" -> t.c.toString,
      "edges" -> t.values.map(_.toString)
    ))
    val waitingBoardTiles = waitingBoard.tileList.map(t => Map(
      "a" -> t.a.toString,
      "b" -> t.b.toString,
      "c" -> t.c.toString,
      "edges" -> t.values.map(_.toString)
    ))
    val gameData = Map(
      "MoveCount" -> this.moveCount.toString(),
      "GameBoard" -> gameBoardTiles,
      "WaitingBoard" -> waitingBoardTiles
    )
    val jsonString = writePretty(gameData)
    val file = new File(filename)
    val writer = new PrintWriter(file)
    writer.write(jsonString)
    writer.close()



  def loadGame(): Unit = ???
  

  /**
    * Advancing the game and increase the moveCount.
    */
  def advance() = 
    this.moveCount += 1

  def status(): String =
    if this.gameBoard.isCompleted then
      "Congratulations! You have won the game!"
    else
      s"Move ${this.moveCount}"

end Game
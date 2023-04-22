package engine

import engine.grid.GridPos

class Game:
  var gameBoard: GameBoard = new GameBoard()
  var waitingBoard: WaitingBoard = new WaitingBoard()
  private var moveCount: Int = 0
  
  def newGame(): Unit =
    this.gameBoard = new GameBoard()
    this.waitingBoard = new WaitingBoard()
    this.gameBoard.generateSolution()
    this.gameBoard.shuffleTiles()
  
  def saveGame(): Unit = ???

  def loadGame(): Unit = ???
  

  /**
    * List of move for advancing the game
    * 
    *   - Exchange two tiles in gameBoard
    *   - Exchange two tiles in waitingBoard
    *   - Exchange two tiles in gameBoard - waitingBoard
    *   - Rotate one TriTile in gameBoard
    *   - Rotate one TriTile in waitingBoard
    */
  def advance(moveType: String) = 
    if (moveType == "exchange: game -> game") then
      val posFrom: GridPos = ???
      val posTo: GridPos = ???
      this.gameBoard.moveTile(this.gameBoard, posFrom, posTo)
    else if (moveType == "move: waiting -> waiting") then
      val posFrom: GridPos = ???
      val posTo: GridPos = ???
      this.waitingBoard.moveTile(this.waitingBoard, posFrom, posTo)
    else if (moveType == "move: game <-> waiting") then
      val posFrom: GridPos = ???
      val posTo: GridPos = ???
      this.gameBoard.exchangeTile(this.waitingBoard, posFrom, posTo)
    else if (moveType == "rotate in gameBoard") then
      val degrees = ???
      val posFrom: GridPos = ???
      val posTo: GridPos = ???
      // degrees match
      //   this.gameBoard.pickTile.rotateClockwise
      //   this.gameBoard.pickTile.rotateCounterClockwise
      //   this.gameBoard.pickTile.flipTri
    else if (moveType == "rotate in waiting") then
      val degrees = ???
      val posFrom: GridPos = ???
      val posTo: GridPos = ???
      // degrees match
      //   this.waitingBoard.pickTile.rotateClockwise
      //   this.waitingBoard.pickTile.rotateCounterClockwise
      //   this.waitingBoard.pickTile.flipTri

    this.moveCount += 1

  def status(): String =
    if this.gameBoard.isCompleted then
      "Congratulations! You have won the game!"
    else
      s"Move ${this.moveCount}"

end Game
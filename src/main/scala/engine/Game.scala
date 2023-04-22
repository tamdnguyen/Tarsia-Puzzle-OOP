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
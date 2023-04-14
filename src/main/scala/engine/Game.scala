package engine

class Game:
  var gameBoard: GameBoard = new GameBoard()
  var waitingBoard: WaitingBoard = new WaitingBoard()
  
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
    *   - Move a TriTile from gameBoard to waitingBoard
    *   - Move a TriTile from waitingBoard to gameBoard
    *   - Exchange two tiles in gameBoard
    *   - Exchange two tiles in waitingBoard
    *   - Rotate one TriTile in gameBoard
    *   - Rotate one TriTile in waitingBoard
    *
    * @return
    */
  def advance() = ???


end Game
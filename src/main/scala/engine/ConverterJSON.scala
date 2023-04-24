package engine

case class GameDataJSON(moveCount: Int,
                        gameBoard: List[TriTileJSON],
                        waitingBoard: List[TriTileJSON])

case class TriTileJSON(a: Int, b: Int, c: Int, edges: List[Int])


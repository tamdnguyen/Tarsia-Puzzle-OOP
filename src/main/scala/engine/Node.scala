// package engine

// import engine.{Board, grid}

// import scala.collection.immutable.TreeSeqMap.Ordering.Iterator.Empty.next


// class Node(val id: Int, var prev: Option[Node] = None, var next: Option[Node] = None)
//   val gameBoard: Board = new GameBoardObject
//   val waitingBoard: WaitingBoard = new WaitingBoardObject

//   def hasNext: Boolean = next.isDefined

//   def hasPrev: Boolean = prev.isDefined

//   def getNext: Option[Node] = next

//   def getPrev: Option[Node] = prev

//   def setNext(node: Option[Node]): Unit = next = node

//   def setPrev(node: Option[Node]): Unit = prev = node


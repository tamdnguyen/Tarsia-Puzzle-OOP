package engine

import engine.grid.Point
import engine.grid.grid._
import scala.util.Random
import scala.annotation.targetName

/** An object of type `Edge` represents an edge of a triangle tile.
 *  Each `Edge` has two `Point`s to represent its position in the grid
 *  and a constant attached to the `Edge` itself.
 *  
 *  An important method of `Edge` is `matchingEdge()`. This method
 *  determines if two edges have the matching value.
 *
 *  Class `Edge` is immutable.
 * */
final case class Edge(val p1: Point, val p2: Point, val value: Int):

  /** Determines whether this edge is identical to another edge.
   *  This is true if and only if they have the same coordinates in
   *  the grid and they have same values.
   * */
  @targetName("equals")
  def ==(another: Edge): Boolean =
    this.matchingCoordinates(another) && this.matchingValue(another) 
    
    
  /** Determines whether this edge is identical to another edge.
   *  This is true if the pair of `Points` in these edges are identical
   *  regardless of the order of the pair.
   *  
   *  For example, Edge made from (1,0) and (0,1) are essentially
   *  identical to the Edge made from (0,1) and (1,0).
   * */
  def matchingCoordinates(another: Edge): Boolean =
    this.identicalPair(another) || this.reversePair(another)


  /** Determines whether this edge is identical to another edge.
   *  This is true if and only if these edges have the same p1 and p2.
   * */
  def identicalPair(another: Edge): Boolean =
    this.p1.==(another.p1) && this.p2.==(another.p2)

  
  /** Determines whether this edge is a reverse of another edge.
   *  This is true if and only if 
   *    - the first point of this edge equals the second point of another edge
   *  AND
   *    - the second point of this edge equals the first point of another edge
   * */
  def reversePair(another: Edge): Boolean =
    this.p1.==(another.p2) && this.p2.==(another.p1)


  /** 
   * Determines whether this edge has the same value with another edge.
   * 
   * If the other edge has value that is not in the list of all edge values,
   * it indicates that the edge neighbor tile is just initialized, and return false.
   * */
  def matchingValue(another: Edge): Boolean =
    if edgeValues.contains(another.value) then
      this.value == this.matchEdgeVal(another.value)
    else
      false
    

  /**
  * Return an `Int` that matches the input.
  *
  * @example{{{
  * matchEdgeVal(1) = 11
  * matchEdgeVal(3) = 33
  * matchEdgeVal(22) = 2
  * matchEdgeVal(44) = 4
  * matchEdgeVal(-1) = random number in (1,2,3,4,11,22,33,44)
  * }}}
  * 
  * @param target the Edge value to be matched
  * @return an `Int` that matches the value of `target`.
  */
  def matchEdgeVal(target: Int): Int =
    if edgeValues.contains(target) then
      if edgeVals1.contains(target) then
        val index = edgeVals1.indexOf(target)
        edgeVals2(index)
      else 
        val index = edgeVals2.indexOf(target)
        edgeVals1(index)
    else 
      this.randEdgeVal()


  /**
    * Returns a random value from the list of possible edge values.
    */
  def randEdgeVal(): Int = 
    val randomIndex = Random.nextInt(edgeValues.length) 
    edgeValues(randomIndex)


  /** Returns a textual description of this edge. The description is of the form `"((x1,y1),(x2,y2): value)"`. */
  override def toString = s"((${this.p1.x},${this.p1.y}), (${this.p2.x},${this.p2.y}): ${this.value})"

end Edge


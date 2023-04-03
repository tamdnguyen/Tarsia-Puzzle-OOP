package engine

import engine.grid.*
import scala.annotation.targetName

/** An object of type `Edge` represents an edge of a triangle tile.
 *  Each `Edge` has two `Point`s to represent its position in the grid
 *  and a constant attached to the `Edge` itself.
 *  
 *  An important method of `Edge` is `matchingEdge()`. This method
 *  determines if two edges have the matching value.
 *
 * The `Point`s of an `Edge` are mutable, but the value is immutable.
 * */
final case class Edge(var p1: Point, var p2: Point, val value: Int):

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


  /** Determines whether this edge has the same value with another edge.
   * */
  def matchingValue(another: Edge): Boolean =
    this.value == another.value
    
    
  /** Returns a textual description of this edge. The description is of the form `"(x1,y1)-(x2,y2)"`. */
  override def toString = s"(${this.p1.x}, ${this.p1.y})-(${this.p2.x}, ${this.p2.y})"

end Edge


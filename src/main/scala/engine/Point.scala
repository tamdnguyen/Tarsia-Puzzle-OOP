package engine

import scala.annotation.targetName

/** An object of type `Point` represents a pair of coordinates. Such a pair can
  * be used to reference a point on a two dimensional Cartersian coordinate system.
  *
  * The coordinate axes are named `x` and `y`. In this coordinate system, `x` increases
  * “eastwards” and y` increases “southwards”.
  *
  * `Point` objects are immutable.
  *
  * @param x  an x coordinate
  * @param y  a y coordinate */
final case class Point(val x: Double, val y: Double):

  /** Determines whether this grid position equals the given one. This is the case if
    * the two have identical x and y coordinates. */
  @targetName("equals")
  def ==(another: Point): Boolean = this.x == another.x && this.y == another.y


  /** Returns the difference between the `x` coordinate of this `Point` and that of the given
    * `Point`. The result is negative if the `x` coordinate of this `Point` is greater. */
  def xDiff(another: Point): Double = another.x - this.x


  /** Returns the difference between the `y` coordinate of this `Point` and that of the given
    * `Point`. The result is negative if the `y` coordinate of this `Point` is greater. */
  def yDiff(another: Point): Double = another.y - this.y


  /** Returns the [[xDiff]] and [[yDiff]] between this `Point` and the given one as a pair. */
  def diff(another: Point): (Double, Double) = (this.xDiff(another), this.yDiff(another))


  /** Returns the “Manhattan distance” between this `Point` and the given one.
    * The Manhattan distance between `a` and `b` equals `a.xDiff(b).abs + a.yDiff(b).abs`. */
  def distance(another: Point): Double = this.xDiff(another).abs + this.yDiff(another).abs


  /** Returns a textual description of this position. The description is of the form `"(x,y)"`. */
  override def toString = s"($x,$y)"

end Point



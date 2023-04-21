package engine.grid

import scala.annotation.targetName

/** An object of type `Point` represents a pair of coordinates. Such a pair can
 * be used to reference a point on a two dimensional Cartersian coordinate system.
 *
 * `Point` objects are immutable.
 *
 * @param x an x coordinate
 * @param y a y coordinate */
final case class Point(val x: Double, val y: Double):

  private val tolerance = 0.001


  /** Determines whether this grid position equals the given one. This is the case if
   * the two have identical x and y coordinates. */
  @targetName("equals")
  def ==(another: Point): Boolean =
    this.xDiff(another).abs <= this.tolerance &&
      this.yDiff(another).abs <= this.tolerance


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


  /**
    * Interface to convert a point in GUI to its corresponding point in engine system.
    * 
    * Essentially, in engine.grid system, the x-axis increases eastward and the y-axis
    * increases northward. The hexagon board is located at (0,0), and other points (corners, edge, ...)
    * are place around that origin (0,0).
    * 
    * However, in swing GUI system, the x-axis increase eastward and the y-axis
    * increase southward, and the top left corner of the any window/panel is
    * point (0,0).
    * 
    * Therefore, to convert a point A from GUI system to a corresponding point in engine.grid,
    * first we need to shift A dx to the left and dy down, then we flip the A over the y-axis.
    *
    * @param dx
    * @param dy
    * @return
    */
  def shiftGUItoEngine(dx: Double, dy: Double): Point = 
    Point(this.x - dx, -(this.y - dy))


  /**
    * Interface to convert a point in engine to its corresponding point in GUI.
    * 
    * Essentially, in engine.grid system, the x-axis increases eastward and the y-axis
    * increases northward. The hexagon board is located at (0,0), and other points (corners, edge, ...)
    * are place around that origin (0,0).
    * 
    * However, in swing GUI system, the x-axis increase eastward and the y-axis
    * increase southward, and the top left corner of the any window/panel is
    * point (0,0).
    * 
    * Therefore, to convert a point A from engine.grid to a corresponding point in GUI system,
    * first we need to shift A dx to the right and flip the A over the y-axis, then we
    * shift A by increasing new y by dy (this will be up in the y-axis in the new system)
    *
    * @param dx
    * @param dy
    * @return
    */
  def shiftEngineToGUI(dx: Double, dy: Double): Point = 
    Point(this.x + dx, -this.y + dy)


  /** Returns a textual description of this position. The description is of the form `"(x,y)"`. */
  override def toString = s"($x,$y)"

end Point

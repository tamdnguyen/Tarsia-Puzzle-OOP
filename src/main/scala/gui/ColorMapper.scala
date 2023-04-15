package gui

import engine.grid.grid._
import java.awt.Color

/**
  * Return the color that matches the value of the edge.
  */
class ColorMapper:

  /**
    * Define base colors for each unique value.
    */
  val baseColors = Map(
    1 -> Color.RED,
    2 -> Color.GREEN,
    3 -> Color.BLUE,
    4 -> Color.YELLOW,
    5 -> Color.ORANGE,
    6 -> Color.PINK,
    11 -> Color.RED.brighter(),
    22 -> Color.GREEN.brighter(),
    33 -> Color.BLUE.brighter(),
    44 -> Color.YELLOW.brighter(),
    55 -> Color.ORANGE.brighter(),
    66 -> Color.PINK.brighter()
  )
  require(baseColors.keys.toSeq.forall(edgeValues.contains(_)), "The ColorMapper is incorrect.")


  /**
    * Return the Color that matches the given edge values.
    */
  def getColor(value: Int): Color = 
    require(this.baseColors.contains(value), "Invalid edge value. Cannot find the matching Color.")
    this.baseColors(value)


  /**
    * Same as method getColor(). This provides the ability to use ColorMapper(value).
    */
  def apply(value: Int): Color =
    this.getColor(value)


end ColorMapper

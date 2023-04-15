package gui

import engine.grid.grid._
import java.awt.Color

/**
  * Return the color that matches the value of the edge.
  */
object ColorMapper:

  /**
    * Define base colors for each unique value.
    */
  val baseColors = Map(
    1 -> Color.RED.darker(),
    2 -> Color.GREEN.darker(),
    3 -> Color.BLUE.darker(),
    4 -> Color.GRAY.darker(),
    5 -> Color.ORANGE.darker(),
    6 -> Color.PINK.darker().darker(),
    11 -> Color.RED.brighter(),
    22 -> Color.GREEN.brighter(),
    33 -> Color.BLUE.brighter(),
    44 -> Color.GRAY.brighter(),
    55 -> Color.ORANGE.brighter(),
    66 -> Color.PINK
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

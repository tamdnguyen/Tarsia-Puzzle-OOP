package gui

import java.awt.Color
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ColorMapperSpec extends AnyFlatSpec with Matchers {

  "ColorMapper" should "return the correct base color for each unique value" in {
    val colorMapper = ColorMapper
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
    colorMapper.baseColors should equal(baseColors)
  }

  it should "only contains the possible values of the edges" in {
    val colorMapper = ColorMapper
    colorMapper.baseColors.keys.toVector.sorted should equal(engine.grid.grid.edgeValues.sorted)
  }


  it should "throw an IllegalArgumentException if the edge value is invalid" in {
    val colorMapper = ColorMapper
    assertThrows[IllegalArgumentException] {
      colorMapper.getColor(7)
    }
  }

  it should "return the correct color for each edge value" in {
    val colorMapper = ColorMapper
    colorMapper.getColor(1) should equal(Color.RED)
    colorMapper.getColor(11) should equal(Color.RED.brighter())
    colorMapper.getColor(2) should equal(Color.GREEN)
    colorMapper.getColor(22) should equal(Color.GREEN.brighter())
    colorMapper.getColor(3) should equal(Color.BLUE)
    colorMapper.getColor(33) should equal(Color.BLUE.brighter())
    colorMapper.getColor(4) should equal(Color.YELLOW)
    colorMapper.getColor(44) should equal(Color.YELLOW.brighter())
    colorMapper.getColor(5) should equal(Color.ORANGE)
    colorMapper.getColor(55) should equal(Color.ORANGE.brighter())
    colorMapper.getColor(6) should equal(Color.PINK)
    colorMapper.getColor(66) should equal(Color.PINK.brighter())
  }

  it should "return the correct color for each edge value using the apply() method" in {
    val colorMapper = ColorMapper
    colorMapper(1) should equal(Color.RED)
    colorMapper(11) should equal(Color.RED.brighter())
    colorMapper(2) should equal(Color.GREEN)
    colorMapper(22) should equal(Color.GREEN.brighter())
    colorMapper(3) should equal(Color.BLUE)
    colorMapper(33) should equal(Color.BLUE.brighter())
    colorMapper(4) should equal(Color.YELLOW)
    colorMapper(44) should equal(Color.YELLOW.brighter())
    colorMapper(5) should equal(Color.ORANGE)
    colorMapper(55) should equal(Color.ORANGE.brighter())
    colorMapper(6) should equal(Color.PINK)
    colorMapper(66) should equal(Color.PINK.brighter())
  }

}

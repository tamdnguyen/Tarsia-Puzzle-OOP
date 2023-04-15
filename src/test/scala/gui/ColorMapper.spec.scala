package gui

import java.awt.Color
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ColorMapperSpec extends AnyFlatSpec with Matchers {

  "ColorMapper" should "only contains the possible values of the edges" in {
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
    colorMapper.getColor(1) should equal(Color.RED.darker())
    colorMapper.getColor(11) should equal(Color.RED.brighter())
    colorMapper.getColor(2) should equal(Color.GREEN.darker())
    colorMapper.getColor(22) should equal(Color.GREEN.brighter())
    colorMapper.getColor(3) should equal(Color.BLUE.darker())
    colorMapper.getColor(33) should equal(Color.BLUE.brighter())
    colorMapper.getColor(4) should equal(Color.GRAY.darker())
    colorMapper.getColor(44) should equal(Color.GRAY.brighter())
    colorMapper.getColor(5) should equal(Color.ORANGE.darker())
    colorMapper.getColor(55) should equal(Color.ORANGE.brighter())
    colorMapper.getColor(6) should equal(Color.PINK.darker().darker())
    colorMapper.getColor(66) should equal(Color.PINK)
  }

  it should "return the correct color for each edge value using the apply() method" in {
    val colorMapper = ColorMapper
    colorMapper(1) should equal(Color.RED.darker())
    colorMapper(11) should equal(Color.RED.brighter())
    colorMapper(2) should equal(Color.GREEN.darker())
    colorMapper(22) should equal(Color.GREEN.brighter())
    colorMapper(3) should equal(Color.BLUE.darker())
    colorMapper(33) should equal(Color.BLUE.brighter())
    colorMapper(4) should equal(Color.GRAY.darker())
    colorMapper(44) should equal(Color.GRAY.brighter())
    colorMapper(5) should equal(Color.ORANGE.darker())
    colorMapper(55) should equal(Color.ORANGE.brighter())
    colorMapper(6) should equal(Color.PINK.darker().darker())
    colorMapper(66) should equal(Color.PINK)
  }

}

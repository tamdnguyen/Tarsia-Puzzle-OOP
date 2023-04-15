package gui.Examples

import scala.swing._
import scala.swing.event._
import java.awt.{Point, Color, Font}
import javax.swing.{ToolTipManager}

object MousePosition extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Mouse Position Demo"
    val label = new Label("Mouse position: ")
    val panel = new Panel {
      preferredSize = new Dimension(300, 300)
      background = Color.ORANGE
      listenTo(mouse.moves)
      reactions += {
        case e: MouseMoved =>
          label.text = s"Mouse position: (${e.point.x}, ${e.point.y})"
          peer.setToolTipText(s"Mouse position: (${e.point.x}, ${e.point.y})")
          repaint()
      }
    }
    contents = new BorderPanel {
      layout(label) = BorderPanel.Position.South
      layout(panel) = BorderPanel.Position.Center
    }
    
    // set tooltip delay
    ToolTipManager.sharedInstance().setInitialDelay(0)
    ToolTipManager.sharedInstance().setDismissDelay(1000)
  }
}

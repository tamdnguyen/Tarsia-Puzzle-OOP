
import engine._
import engine.grid._
import gui._
import scala.swing._

object GameApp extends SimpleSwingApplication:

  // Set the triangle edge length
  engine.grid.grid.edgeLength = boardHexagonSize / 4

  val game = new Game()
  val gui = new GameGUI(game)

  def top: MainFrame = this.gui

end GameApp
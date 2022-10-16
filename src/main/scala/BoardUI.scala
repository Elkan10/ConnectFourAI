import java.awt.Color
import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event.MouseClicked

class BoardUI(board: Board) extends GridPanel(6,7) {
  var click: (Int, Int) = (-1,-1)
  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(_,p,_,_,_) => click = (p.x,p.y)
  }
  for(comp<-display(board)) {
    contents += comp
  }
  def display(board: Board): List[Component] = {
    val out = new ListBuffer[Component]
    val s = new Square()
    var p = 0
    for (i <- 0 until 7 * 6) {
      val square = board.squares(p)
      if (square == s.RED) {
        out += Circle(0,0,Color.RED)
      }
      else if (square == s.YELLOW) {
        out += Circle(0,0,Color.YELLOW)
      }
      else {
        out += Circle(0,0,Color.GRAY)
      }
      if (p >= 36) {
        p -= 35 + 6
      }
      p += 6
    }
    out.toList
  }
}

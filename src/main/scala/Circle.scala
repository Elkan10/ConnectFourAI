import scala.swing._

case class Circle(x:Int,y:Int,color:Color) extends Component {
  override def paintComponent(g:Graphics2D): Unit = {
    super.paintComponent(g)
    g.setColor(color)
    g.fillOval(x,y,80,80)
  }

}

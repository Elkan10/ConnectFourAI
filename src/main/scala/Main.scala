import java.io.File


object Main {
  def main(args:Array[String]): Unit = {
    val file = new File("res/pos_data.bin")
    if(!file.exists()) {
      file.createNewFile()
    }
    val board = new Board()
    board.readMap(file)
    val ui = new UI
    ui.contents = new BoardUI(board)
    ui.resizable = false
    ui.visible = true
    val minimax = new Minimax
    var turns = 0
    while(!board.checkWin()) {
      val p1 = minimax.findBestMove(board,8,max = true)
      board.makeMove(p1,isRed = true)
      val c = new BoardUI(board)
      ui.contents = c
      if(board.checkWin()) {
        board.writeMap(file)
        print("AI won!")
        c.deafTo(c.mouse.clicks)
      }

      while(c.click == (-1,-1)) {}
      val p2 = c.click._1/100
      board.makeMove(p2, isRed = false)
      ui.contents = new BoardUI(board)
      turns += 1
    }
    board.writeMap(file)
    print("You won!")
  }
}

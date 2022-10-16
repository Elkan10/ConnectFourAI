import java.io._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Board {
  var squares = createBoard()
  var map = new mutable.HashMap[Int,Int]()
  def createBoard(): List[Square] = {
    val s = new Square()
    val buf = new ListBuffer[Square]
    //y-first
    for(i <- 1 to 7*6) {
      buf.addOne(s.NONE)
    }
    buf.toList
  }

  def mapped(): Boolean = {
    map.contains(serialize())
  }

  def mappedMove(): Int = {
    map(serialize())
  }

  def addMappedMove(move: Int): Unit = {
    map.put(serialize(), move)
  }

  def writeMap(file: File): Unit = {
    try {
      val fos = new FileOutputStream(file)
      val oos = new ObjectOutputStream(fos)
      oos.writeObject(map)
      oos.close()
    } catch {
      case _:IOException=>println("Couldn't write data")
    }
  }

  def readMap(file: File): Unit = {
    try {
      val fis = new FileInputStream(file)
      val ois = new ObjectInputStream(fis)
      map = ois.readObject().asInstanceOf[mutable.HashMap[Int,Int]]
      ois.close()
    } catch {
      case _: IOException => println("Couldn't read data")
    }
  }

  def legalMoves(): List[Int] = {
    val s = new Square()
    val buf = new ListBuffer[Int]
    for(pos<-0 to 6) {
      if(squares(pos*6) == s.NONE) {
        buf.addOne(pos)
      }
    }
    buf.toList
  }

  def sortedLegalMoves(isRed:Boolean, p:Boolean = false): List[Int] = {
    legalMoves()
  }

  def eval(): Int = {
    var ans = 0
    for (p <- 0 to 41) {
      ans += check(p)
    }
    ans
  }

  def makeMove(pos:Int, isRed:Boolean): Unit = {
    val s = new Square()
    if(!legalMoves().contains(pos)) {
      return
    }
    var new_pos = pos*6
    for (i <- 1 to 5) {
      if(squares((pos*6)+i) == s.NONE) {
        new_pos = (pos*6)+i
      }
    }
    val out = new ListBuffer[Square]
    var p = 0
    for(square<-squares) {
      if(p != new_pos) {
        out.addOne(square)
      } else {
        if(isRed) {
          out.addOne(s.RED)
        } else {
          out.addOne(s.YELLOW)
        }
      }
      p += 1
    }
    squares = out.toList
  }

  def withMove(pos: Int, isRed: Boolean): Board = {
    val out = new Board
    val buf = new ListBuffer[Square]
    for(square<-squares) {
      buf.addOne(square)
    }
    out.squares = buf.toList
    out.makeMove(pos, isRed)
    out
  }

  def checkWin(): Boolean = {
    for(p<-0 to 41) {
      if(Math.abs(check(p)) == 20) return true
    }
    false
  }

  def findLen(l:List[Square]): Int = {
    val r_i = l.map(a => a.isRed.compareTo(false))
    val y_i = l.map(a => a.isYellow.compareTo(false))
    val r = r_i.sum
    val y = y_i.sum

    if(r == 0) {
      return -y
    }
    if(y == 0) {
      return r
    }
    0
  }


  def check(p:Int): Int = {

    var sum = 0
    var hor, ver, diaR, diaL = 0

    //Horizontal
    if (p < 24) {
      val l = new ListBuffer[Square]
      l.addOne(squares(p))
      l.addOne(squares(p + 6))
      l.addOne(squares(p + 12))
      l.addOne(squares(p + 18))
      hor = findLen(l.toList)
      if(math.abs(hor) == 4) return hor*5
      sum += hor
    }
    //Top Check
    if(p % 6 < 3) {
      //Vertical
      {
        val l = new ListBuffer[Square]
        l.addOne(squares(p))
        l.addOne(squares(p + 1))
        l.addOne(squares(p + 2))
        l.addOne(squares(p + 3))
        ver = findLen(l.toList)
        if(math.abs(ver) == 4) return ver*5
        sum += ver
      }
      //Diagonal Right
      if(p < 24) {
        val l = new ListBuffer[Square]
        l.addOne(squares(p))
        l.addOne(squares(p + 7))
        l.addOne(squares(p + 14))
        l.addOne(squares(p + 21))
        diaR = findLen(l.toList)
        if (math.abs(diaR) == 4) return diaR*5
        sum += diaR
      }
      //Diagonal Left
      if(p >= 18) {
        val l = new ListBuffer[Square]
        l.addOne(squares(p))
        l.addOne(squares(p - 5))
        l.addOne(squares(p - 10))
        l.addOne(squares(p - 15))
        diaL = findLen(l.toList)
        if (math.abs(diaL) == 4) return diaL*5
        sum += diaL
      }
    }
    sum
  }
  def serialize(): Int = {
    squares.hashCode()
  }
}

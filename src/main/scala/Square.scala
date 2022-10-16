case class Square(isRed:Boolean, isYellow:Boolean) {
  def RED: Square = Square(isRed = true, isYellow = false)
  def YELLOW: Square = Square(isRed = false, isYellow = true)
  def NONE: Square = Square(isRed = false,isYellow = false)
  def this() {
    this(false,false)
  }
}


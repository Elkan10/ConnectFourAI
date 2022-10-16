

class Minimax {
  def findBestMove(pos: Board, depth: Int, max: Boolean): Int = {
    if(pos.mapped()) {
      return pos.mappedMove()
    }
    if(max) {
      var maxEval = Int.MinValue
      var bestMove:Int = 3
      for (move <- pos.sortedLegalMoves(isRed = true,p = true)) {
        val eval = minimax(pos.withMove(move, isRed = true), depth - 1, max = false, Int.MinValue, Int.MaxValue)
        if(eval > maxEval) {
          maxEval = eval
          bestMove = move
        }
      }
      pos.addMappedMove(bestMove)
      bestMove
    } else {
      var minEval = Int.MaxValue
      var bestMove: Int = 3
      for (move <- pos.sortedLegalMoves(isRed = false)) {
        val eval = minimax(pos.withMove(move, isRed = false), depth - 1, max = true, Int.MinValue, Int.MaxValue)
        if (eval < minEval) {
          minEval = eval
          bestMove = move
        }
      }
      pos.addMappedMove(bestMove)
      bestMove
    }
  }
  def minimax(pos: Board, depth: Int, max: Boolean, alpha: Int, beta: Int): Int = {
    var new_alpha = alpha
    var new_beta = beta
    if(depth == 0 || pos.checkWin()) {
      return pos.eval()
    }
    if(max) {
      var maxEval = Int.MinValue
      var done = false
      for(move<-pos.sortedLegalMoves(isRed = true)) {
        if(!done) {
          val eval = minimax(pos.withMove(move,isRed = true), depth-1, max = false, new_alpha, new_beta)
          maxEval = Math.max(maxEval, eval)
          new_alpha = Math.max(new_alpha, eval)
          if(new_beta <= new_alpha) {
            done = true
          }
        }
      }
      maxEval
    }
    else {
      var minEval = Int.MaxValue
      var done = false
      for (move <- pos.sortedLegalMoves(isRed = false)) {
        if(!done) {
          val eval = minimax(pos.withMove(move, isRed = false), depth - 1, max = true, new_alpha, new_beta)
          minEval = Math.min(minEval, eval)
          new_beta = Math.min(new_beta, eval)
          if(new_beta <= new_alpha) {
            done = true
          }
        }
      }
      minEval
    }
  }
}

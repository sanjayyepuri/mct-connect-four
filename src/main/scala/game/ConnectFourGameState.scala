package game
import scala.collection.mutable.LinkedList

class ConnectFourGameState(val board: Array[Array[Int]], val player: Int, val lastMove: Action = null) extends GameState(board, player) {
    private val Row = 6;
    private val Column = 7;
    private def nextPlayer = player match { case 1 => -1; case -1 => 1 }

    private def isLegalMove(move: Action):Boolean = {
        if(move.value != player) false 
        else if (0 > move.column && move.column >= board(0).length) false
        else if (board(board.length-1)(move.column) != 0) false // if the column is filled
        else true  
    }

    private def getTop(col: Int) = {
        def recur(index: Int):Int = board(index)(col) match {
            case v if v == 0 && index > 0 => recur(index - 1)
            case v if v != 0 => index + 1
            case _ => -1
        }
        recur(board.length-1)
    }
    
    private def checkBounds(r:Int, c:Int):Boolean = { 
        if(c < 0 || c >= board.length || r < 0 || r >= board(c).length) false 
        else true
    }

    def gameOver: Boolean = gameResult != None
    
    def gameResult: Result = {
        if(lastMove == null) return None
        
        var win: Boolean = false
        val moveRow = getTop(lastMove.column)
        val moveCol = lastMove.column
        val coin = board(moveRow)(lastMove.column)

        def checkRow(r: Int, count: Int = 0): Boolean = {
            if(count >= 4) true
            else if(r+1 < Row) checkRow(r+1, count + (if(board(r)(moveCol) == coin) 1 else 0))
            else false
        }

        def checkColumn(c: Int, count: Int = 0): Boolean = {
            if(count >= 4) true 
            else if(c+1 < Column) checkColumn(c+1, count + (if(board(moveRow)(c) == coin) 1 else 0))
            else false 
        }

        def checkDiagonalFromTop(r: Int, c: Int, rMax: Int, cMax: Int, count: Int = 0): Boolean  = {
            if(count >= 4) true
            else if(r+1 < rMax && c+1 < cMax) {
                if(checkBounds(r, c)) checkDiagonalFromTop(r+1, c+1, rMax, cMax, count + (if(board(r)(c) == coin) 1 else 0))
                else checkDiagonalFromTop(r+1, c+1, rMax, cMax, count)
            }
            else false
        }
        def checkDiagonalFromBottom(r: Int, c: Int, rMax: Int, cMax: Int, count: Int = 0): Boolean = {
            if(count >= 4) true
            else if(r-1 > rMax && c+1 < cMax) {
                if(checkBounds(r, c)) checkDiagonalFromBottom(r-1, c+1, rMax, cMax, count + (if(board(r)(c) == coin) 1 else 0))
                else checkDiagonalFromBottom(r-1, c+1, rMax, cMax, count)
            }
            else false
        }

        win = checkRow(0) | checkColumn(0) | 
            checkDiagonalFromBottom(moveRow-7, moveCol-7, moveRow+7, moveCol+7) |
            checkDiagonalFromTop(moveRow+7, moveCol-7, moveRow-7, moveCol+7);
        
        if(win) coin match {
            case -1 => RedWin
            case 1 => BlueWin
            case _ => throw new IllegalStateException("Illegal winner")
        } else if(getLegalActions.size == 0) {
            Tie 
        } else None
    }
    
    def getLegalActions(): List[Action] = {
        // TODO: this can be probably done better
        val legalActions = (0 to (board(0).length-1))
          .map((col: Int) => new Action(player, col))
          .filter(isLegalMove)

        legalActions.toList
    }
 
    def move(move: Action): ConnectFourGameState = {
        if(!isLegalMove(move)) throw new IllegalArgumentException("Invalid move")
        
        val nextState = board.map(_.clone)
        val top = getTop(move.column)  
        
        nextState(top)(move.column) = nextPlayer
        new ConnectFourGameState(nextState, nextPlayer, move)
    }

    override def toString: String = { 
        board.map(r => r.mkString(", ")).mkString("\n")
    }
}



package game
import scala.collection.mutable.LinkedList

class ConnectFourGameState(val state: Array[Array[Int]], val player: Int) extends GameState(state, player) {

    private def nextPlayer = player match { case 1 => -1; case -1 => 1 }

    private def isLegalMove(move: Action):Boolean = {
        if(move.value != player) false 
        else if (0 > move.column && move.column >= state(0).length) false
        else if (state(state.length-1)(move.column) != 0) false // if the column is filled
        else true  
    }

    def gameOver(): Boolean = gameResult != None
    
    def gameResult: Result = {
        None // TODO
    }
    
    def getLegalActions(): List[Action] = {
        // TODO: this can be probably done better
        val legalActions = (0 to (state(0).length-1)).map((col: Int) => new Action(player, col)).filter(isLegalMove)

        legalActions.toList
    }
 
    def move(move: Action): ConnectFourGameState = {
        if(!isLegalMove(move)) throw new IllegalArgumentException("Invalid move")
        
        val nextState = state.map(_.clone)
        val top: Int = {
            def recur(index: Int):Int = state(index)(move.column) match {
                case v if v == 0 && index > 0 => recur(index-1)
                case v if v != 0 => index+1
                case _ => -1
            }

            recur(state.length-1);
        }
        
        nextState(top)(move.column) = nextPlayer
        new ConnectFourGameState(nextState, nextPlayer)
    }

    override def toString: String = { 
        state.map(r => r.mkString(", ")).mkString("\n")
    }
}



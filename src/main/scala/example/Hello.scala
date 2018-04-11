package example
import game._

object Hello {
  def main(args: Array[String]): Unit = {
    val state = Array(Array(1, 0, 0, 0, 0, 0, 0),
                      Array(1, 0, 0, 0, 0, 0, 0),
                      Array(0, 0, 0, 0, 0, 0, 0),
                      Array(0, 0, 0, 0, 0, 0, 0),
                      Array(0, 0, 0, 0, 0, 0, 0),
                      Array(0, 0, 0, 0, 0, 0, 0))
    val c4State = new ConnectFourGameState(state, 1)
    val move = new Action(1, 0)
    val nextState = c4State.move(move) 
    println(c4State)
    println()
    println(nextState)
    println(c4State.getLegalActions())
    println(nextState.state(2)(0))

  }
}


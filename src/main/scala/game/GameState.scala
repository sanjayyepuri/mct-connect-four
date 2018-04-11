package game 

abstract class GameState(state: Array[Array[Int]], player: Int) { 
  def gameResult: Result
  def gameOver(): Boolean
  def move(action: Action): GameState
  def getLegalActions(): List[Action]
}
class Action (val value: Int, val column: Int) {
  override def toString: String = s"Action(player=$value, column=$column)"
}

sealed trait Result
case object RedWin extends Result
case object BlueWin extends Result
case object Tie extends Result
case object None extends Result

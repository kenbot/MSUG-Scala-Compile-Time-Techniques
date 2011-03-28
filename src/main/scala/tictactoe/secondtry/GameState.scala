package tictactoe.secondtry


sealed trait GameState
case class GameWon(winner: Player) extends GameState
case object GameDrawn extends GameState
case class GameInProgress(newBoard: Board) extends GameState
case object IllegalMove extends GameState



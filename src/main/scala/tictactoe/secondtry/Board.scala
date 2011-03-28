package tictactoe.secondtry


object Board {
  def createBoard(size: Int, players: Player*): Option[Board] = 
    if (size >= 1 && players.nonEmpty && players.size == players.distinct.size) 
      Some(new Board(size, players.toList))
    else 
      None

}

sealed class Board private(val size: Int, val players: List[Player]) { 
  self =>
  
  type Position = (Int, Int)
  type Grid = Map[Position, Player]

  val previousBoard: Option[Board] = None
  
  protected def nextPlayers = players
  def whoseTurn = nextPlayers.head
  def previousTurn: Option[Player] = None

  protected val grid: Grid = Map.empty
 
 
  def apply(pos: Position): Option[Player] = grid get pos

  def move(pos: Position): GameState = {
    val validPosition = (coords contains pos._1) && 
                        (coords contains pos._2) &&
                        !(grid contains pos)
                                    
    if (validPosition) {
      val newGrid = grid + (pos -> whoseTurn)
      if (isWin(newGrid, whoseTurn)) GameWon(whoseTurn)
      else if (isDraw(newGrid)) GameDrawn
      else GameInProgress(createBoard(newGrid))
    }
    else IllegalMove 
  }
  
 
  private def createBoard(newGrid: Grid): Board = new Board(size, players) {
    override val grid = newGrid
    override protected def nextPlayers = self.nextPlayers.tail match {
      case tail @ (next :: rest) => tail
      case Nil => players
    }
    override val previousBoard = Some(self)
  }

  private def coords = (1 to size).toList

  private def isWin(grid: Grid, forPlayer: Player): Boolean = {
    val rowList = coords.map(r => coords.map(c => (r,c)))
    val colList = rowList.map(_.map(_.swap))
    val diag1 = coords zip coords
    val diag2 = coords zip coords.reverse
    val winCombos = diag1 :: diag2 :: rowList ::: colList

    winCombos exists (_ map grid.get forall (Some(forPlayer) ==))
  }
  
  private def isDraw(grid: Grid): Boolean = {
    val squares = for (r <- coords; c <- coords) yield (r, c) 
    squares forall grid.contains
  }

  override def toString() = {
    val columnHeader = coords.mkString("   ", "   ", " ")
    val border: String = " " + coords.map(c => ".===").mkString + "."
    
    def row(r: Int): String = {
      def symbol(r: Int, c: Int) = grid.get(r,c).map(_.symbol).getOrElse(' ')
      coords.map(c => symbol(r,c)).mkString(r + "| ", " | ", " |")
    }
    
    val rowList = columnHeader :: coords.flatMap(r => border :: row(r) :: Nil) ::: border :: Nil
    rowList.mkString("\n", "\n", "\n")
  }
}


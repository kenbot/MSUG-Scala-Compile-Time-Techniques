package tictactoe.firsttry


sealed trait Place {
  type not <: Place
}

trait NotX extends Place
trait NotO extends Place
trait NotEmpty extends Place

sealed trait Player extends Place { type nextPlayer <: Player }
class X extends Player with NotEmpty with NotO { type nextPlayer = O; type not = NotX }
class O extends Player with NotEmpty with NotX { type nextPlayer = X; type not = NotO }
class Empty extends Place with NotX with NotO { type not = NotEmpty }

sealed trait Position
trait NW[+p <: Place] extends Position
trait N[+p <: Place] extends Position
trait NE[+p <: Place] extends Position
trait W[+p <: Place] extends Position
trait C[+p <: Place] extends Position
trait E[+p <: Place] extends Position
trait SW[+p <: Place] extends Position
trait S[+p <: Place] extends Position
trait SE[+p <: Place] extends Position

sealed trait Board {self =>

  type whoseTurn <: Player
  type whoWon <: Place
  
  type Grid = {
    type NW <: Place; type N <: Place; type NE <: Place;
    type  W <: Place; type C <: Place; type  E <: Place;
    type SW <: Place; type S <: Place; type SE <: Place;
    
    def NW: NW; def N: N; def NE: NE;
    def  W:  W; def C: C; def  E:  E;
    def SW: SW; def S: S; def SE: SE;
  }
  
  val grid: Grid 
  
  final type WinWestColumn[P <: Player] = Grid {type NW=P; type W=P; type SW=P;} //grid with NW[P] with W[P] with SW[P]
  /* final type WinCentreColumn[P <: Player] = grid with N[P] with C[P] with S[P]
  final type WinEastColumn[P <: Player] = grid with NE[P] with E[P] with SE[P]
  final type WinNorthRow[P <: Player] = grid with NW[P] with N[P] with NE[P]
  final type WinCentreRow[P <: Player] = grid with W[P] with C[P] with E[P]
  final type WinSouthRow[P <: Player] = grid with W[P] with C[P] with E[P]
  final type WinForwardDiagonal[P <: Player] = grid with SW[P] with C[P] with NE[P]
  final type WinRisingDiagonal[P <: Player] = grid with SW[P] with C[P] with NE[P]*/
 
  def move[P <: Position](at: Position)
  
  /*
  def move[at[_ <: Place] <: Position](implicit ev: grid <:< at[Empty]) = new Board {
    type whoseTurn = self.whoseTurn#nextPlayer
    type grid = self.grid with at[self.whoseTurn]
  }*/
}
/*
class EmptyBoard[first <: Player] extends Board {
  type whoseTurn = first
  type whoWon = Empty
  type grid = NW[Empty] with N[Empty] with NE[Empty] with 
               W[Empty] with C[Empty] with  E[Empty] with 
              SW[Empty] with S[Empty] with SE[Empty]
  val grid: grid 
  def move[P <: Position](at: Position) {}
}*/


object Board {


  
  type BoardEmptyAt[at[_ <: Place] <: Position] = Board {
    type grid <: at[Empty]
  }
/*
  implicit def supportMove[at[_ <: Place] <: Position, board <: BoardEmptyAt[at]](current: board) = new {
    def move[to[x <: Place] <: at[x]] = new Board {
      type whoseTurn = board#whoseTurn#nextPlayer
      type grid = board#grid with to[board#whoseTurn]
    }
  }
  */
  


  //def startGame[first <: Player] = new EmptyBoard[first]
}





  /* {
    type NW <: Place
    type x1y2 <: Place
    type x1y3 <: Place
    type x2y1 <: Place
    type x2y2 <: Place
    type x2y3 <: Place
    type x3y1 <: Place
    type x3y2 <: Place
    type x3y3 <: Place
  }
  
  type WinColumn1[p <: Player] = Places {
    type x1y1 = p
    type x1y2 = p
    type x1y3 = p
    type x2y1 <: p.not
    type x2y2 <: p.not
    type x2y3 <: p.not
    type x3y1 <: p.not
    type x3y2 <: p.not
    type x3y3 <: p.not
  }
  
  type WinColumn2[p <: Player] = Places {
    type x1y1 <: p.not
    type x1y2 <: p.not
    type x1y3 <: p.not
    type x2y1 <: p
    type x2y2 <: p
    type x2y3 <: p
    type x3y1 <: p.not
    type x3y2 <: p.not
    type x3y3 <: p.not
  }
  
  type WinColumn3[p <: Player] = Places {
    type x1y1 <: p.not
    type x1y2 <: p.not
    type x1y3 <: p.not
    type x2y1 <: p.not
    type x2y2 <: p.not
    type x2y3 <: p.not
    type x3y1 = p
    type x3y2 = p
    type x3y3 = p
  }
  
  type NextTurn = Board {
    type whoseTurn = self.whoseTurn#other
    type whoWon = self.whoWon
  }
  
  def move(x: Int, y: Int) = {
    (x,y) match {
      case (1,1) => new NextTurn {
        type Places = self.Places {x1y1 = whoseTurn}
      }
      case (1,2) => new NextTurn {
        type Places = self.Places {x1y2 = whoseTurn}
      }
      case (1,3) => new NextTurn {
        type Places = self.Places {x1y3 = whoseTurn}
      }
      case (2,1) => new NextTurn {
        type Places = self.Places {x2y1 = whoseTurn}
      }
      case (2,2) => new NextTurn {
        type Places = self.Places {x2y2 = whoseTurn}
      }
      case (2,3) => new NextTurn {
        type Places = self.Places {x2y3 = whoseTurn}
      }
      case (3,1) => new NextTurn {
        type Places = self.Places {x3y1 = whoseTurn}
      }
      case (3,2) => new NextTurn {
        type Places = self.Places {x3y2 = whoseTurn}
      }
      case (3,3) => new NextTurn {
        type Places = self.Places {x3y3 = whoseTurn}
      }
    }
  }
}*/
/*
trait Board {

  val width, height: Int


  
  
  
  sealed trait Position {
    val x, y: Int
  }

  object Position {
    private[this] case class PositionImpl(x: Int, y: Int) extends Position
    def apply(x: Int, y: Int): Option[Position] = {
      if (x >= 0 && x < width && y >= 0 && y < width) Some(PositionImpl(x,y))
      else None
    }
  }


  def finished: Boolean
  
  def isOccupied(x: Int, y:Int): Boolean

  def move(pos: Position): Board
  def whoWon: Player
  
  def takeBack: Board

  def playerAt(pos: Position): Player
}*/


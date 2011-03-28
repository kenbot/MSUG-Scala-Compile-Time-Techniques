
package tictactoe.secondtry

import org.scalatest._
import matchers._



class BoardSpec extends Spec with ShouldMatchers {
  object X extends Player('X')
  object O extends Player('O')
  object A extends Player('A')
  object B extends Player('B')
  val newBoard = Board.createBoard(3, X, O).get

  describe ("Board") {
    it("should have the expected size and players") {
      newBoard should have (
        'size (3),
        'players (List(X,O))
      )
    }
    
    it("shouldn't be constructable with duplicate players") {
      Board.createBoard(3, X, X) should equal (None)
    }
    
    it("shouldn't be constructable with 0 size") {
      Board.createBoard(0, X, O) should equal (None)
    }
    
    it("shouldn't be constructable with negative size") {
      Board.createBoard(-5, X, O) should equal (None)
    }
    
    it("shouldn't be constructable with no players") {
      Board.createBoard(3) should equal (None)
    }
  }
  
  describe("moving") {
    it("should result in a correct new board") {
      val pos = (1,1)
      newBoard move pos match {
        case GameInProgress(nextBoard) => 
          nextBoard(pos) should equal (Some(newBoard.players.head))
        case fail => error("Game should be in progress, but found state: " + fail)
      }
    }
    
    it("should move to the next player's turn") {
      val pos = (1,1)
      newBoard.whoseTurn should equal (X)
      newBoard move pos match {
        case GameInProgress(nextBoard) => nextBoard.whoseTurn should equal (O)
        case fail => error("Game should be in progress, but found state: " + fail)
      }
    }

    describe("and filling") {
      val Some(b) = Board.createBoard(2, X)
      val GameInProgress(b2) = b.move(1,1)
      def testWin(state: GameState, toWin: Player) = state match {
        case GameWon(player) => player should equal (toWin)
        case fail => error("Game should have been won, but found state: " + fail)
      }
      it("a row should win") {testWin(b2.move(2,1), X)}
      it("a column should win") {testWin(b2.move(1,2), X)}
      it("a diagonal should win") {testWin(b2.move(2,2), X)}
    }

    it("should draw when the board is filled up without winning") {
      val Some(b1) = Board.createBoard(2, X, O, A, B)
      val GameInProgress(b2) = b1.move(1,1)
      val GameInProgress(b3) = b2.move(1,2)
      val GameInProgress(b4) = b3.move(2,1)
      b4.move(2,2) match {
        case GameDrawn => "ok"
        case fail => error("Game should have been drawn, but found state: " + fail)
      }
    }
  }

}

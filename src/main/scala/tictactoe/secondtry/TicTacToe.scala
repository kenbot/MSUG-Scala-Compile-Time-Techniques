package tictactoe.secondtry
import Console.readLine

object TicTacToe extends Application {
  val defaultSize = 3
  val defaultPlayer1 = Player('X')
  val defaultPlayer2 = Player('O')
  val exitShortcut = "exit"

  startMenu()
  
    
  def prompt(message: String) = {
    println(message)
    print("\nTicTacToe> ")
  }

  def readValue[V](msg: String)(validate: String => Either[String, V]): V = {
    prompt(msg)
    val in = readLine.trim
    if (in == exitShortcut) exit(0)
    
    validate(in).fold(err => readValue(err)(validate),  v => v)
  }

  def startMenu() {
    println("Welcome to TicTacToe!")
    setupGameMenu()
  }
  
  def setupGameMenu() {
    val size = askForGridSize()
    println("Using grid size " + size + ".")
    
    val players = askForPlayers()
    println("Using players " + players.mkString(", ") + ".")
    
    playGameMenu(size, players)
  }
  
  def playGameMenu(size: Int, players: List[Player]) {
    val board = Board.createBoard(size, players: _*)
    board match {
      case Some(b) => 
        println(b)
        moveMenu(b)
      case None => 
        println("Couldn't create board for these settings. ");
        setupGameMenu()
    }
  }

  
  def moveMenu(board: Board) {
    def validateCoord(in: String): Either[String, Int] = {
      try Right(in.trim.toInt)
      catch {case _ => Left("That wasn't even an integer!")}
    }
    val r: Int = readValue("Move " + board.whoseTurn + " to row: ")(validateCoord)
    val c: Int = readValue("...and column: ")(validateCoord)
    
    board.move(r,c) match {
      case GameInProgress(b) => 
        println(b)
        moveMenu(b)
        
      case IllegalMove => 
        println("Illegal move!")
        moveMenu(board)
        
      case GameWon(winner) => 
        println(winner + " won the game!")
        playAgainMenu()
        
      case GameDrawn => 
        println("The game is a draw!")
        playAgainMenu()
    }
  }
  
  def playAgainMenu() {
    val playAgain: Boolean = readValue("Play again (Y/N)?") {in => 
      if (in.toUpperCase == "Y") Right(true)
      else if (in.toUpperCase == "N") Right(false)
      else Left("Please enter Y or N.")
    }
    
    if (playAgain) setupGameMenu()
    else exit()
  }

  def askForGridSize(): Int = {
    readValue("Please enter the grid size (default " + defaultSize + "): ") {in => 
      if (in == "") Right(defaultSize)
      else try { 
        val i = in.toInt
        if (i >= 1) Right(i)
        else Left("Size must be greater than 0.")
      }
      catch {
        case _ => Left("Invalid number!")
      }
    }
  }
  
  def askForPlayerOr(msg: String, existing: List[Player], default: Player): Player = 
      askForPlayer(msg + " (" + default.symbol + ")", existing) getOrElse default
  
  def askForPlayer(msg: String, existing: List[Player]): Option[Player] = {
    val errorMsg = Left("Please enter a symbol from A-Z.")
    
    readValue(msg + ": ") {in => 
      if (in.trim.size == 1) {
        val sym: Char = in.trim.head.toUpper
        if ('A' to 'Z' contains sym) {
          val p = Player(sym)
          if (existing contains p) Left("That player already exists.")
          else Right(Some(p))
        }
        else errorMsg
      }
      else if (in.trim.size > 1) Left("One character only please, from A-Z.")
      else if (in.trim.size == 0) Right(None)
      else errorMsg
    }
  }

  
  def askForPlayers(): List[Player] = {
    val p1 = askForPlayerOr("Please enter the first player's symbol from A-Z", Nil, defaultPlayer1)
    println("Players so far: " + p1)
    
    val p2 = askForPlayerOr("Please enter the second player's symbol from A-Z", List(p1), defaultPlayer2)

    def askForPlayerList(existing: List[Player]): List[Player] = {
      println("Players so far: " + existing.mkString(", "))
      askForPlayer("Please enter the next player's symbol from A-Z", existing) match {
        case None => existing
        case Some(p) => askForPlayerList(p :: existing)
      }
    }

    askForPlayerList(p2 :: p1 :: Nil).reverse
  }

  
}
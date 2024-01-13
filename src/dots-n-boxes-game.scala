package game

import scala.io.StdIn
import scala.language.postfixOps
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.annotation.tailrec


////GameStyle //
// this trait class is for all methods using a play method. This will be different for AI and human so they had to have an interface.
trait GameStyle {
  def play(state: GameState): (Int, Int)
}

// this is the main function if you will which is why it extends APP to run this code
object DotsNBoxesGame extends App {
  println(" ")
  println("Choose a ply number")
  val plyNum = StdIn.readInt()
  println(" ")
  println("Choose a board size")
  val boardSize = StdIn.readInt()
  val squared = boardSize * boardSize
  println(" ")
// num from 0-5 w/ filter of 0
  private def genVal(spaces: Int, listVals: List[Int]): List[Int] = {
    if (spaces == 1) return listVals
    val boxVals = 1 + (scala.util.Random.nextInt(5))
    val newList = List(boxVals) ::: genVal(spaces - 1, listVals)
    //println(newList)
    return newList
  }
  // Generates all the values for number of boxes needed based on size and places them in a list.
  val populateVals = {
    val init = 1 + (scala.util.Random.nextInt(5))
    genVal((squared), List(init))
  }


//var nextPartList = populateVals.tail
// The function takes two params the list created from PopVals, and every time a box is created, it will choose the next index to return
    // from squareList so that box can have a value
  private def placeVal(squareList: List[Int], index: Int): Int = {
    //println(squareList(index))
    squareList(index)
  }
  // This code simplifies the parameters within the box class and only passes the index 
  def randomVal(i: Int) = {
    if (i == 0)
      placeVal(populateVals, i)

    placeVal(populateVals.tail, i)

  }

  // the Two players to play the game
  val playerOne = new Human()
  val playerTwo = new Minimax()

  // makes a new game with 1 game, GameStyle 1 (player) , vs GameStyle (minimax)
  new GameRound(1, playerOne, playerTwo).play()

}
/////////////////Game////////////
////////////////////////////////


// ensure our functions are tail recursive

// we make a class with Game with our both strategies, with our extension of Strat
class SucessorFunction(val GameStyle1: GameStyle, val GameStyle2: GameStyle) { // /** we have to make sure we don't do import of strat */
  // makinga board with size 4 (ensure we can ask the user with args or with text)

  val board = new Board(DotsNBoxesGame.boardSize + 1, List.empty)

  // chooses a random player to go first
  val whoStarts = scala.util.Random.nextBoolean()
  // GameStyle picked, WITH SCORE! , and if they go first or not
  val playerOne = new Player(GameStyle1, 0, whoStarts)
  val playerTwo = new Player(GameStyle2, 0, !whoStarts)

  // makes a new game board with a game state
  val gameState = new GameState(board, playerOne, playerTwo)

// the game funtion where it loops the game state
  def play: GameState = gameLoop(gameState)

  @tailrec
  private def gameLoop(state: GameState): GameState = {
    println(state.board)
    println()
    if (state.isTerminal) return state
    val move = state.currentPlayer.pickPlay(state)
    val newState = state.makeMove(move)
    gameLoop(newState)
  }
}

/////////////////////////
//BOARD.scala
/////////////////////////////

// A board object in how the board will be represented within the terminal or output
object Board {

  /**
   Apply function for board for pattern match of size and number of plies */
  def apply(size: Int, plays: List[(Int, Int)]): Board = {
    new Board(size, plays)
  }

  /**  Apply function for board and its viwing proably output wise*/
  def apply(stringRepresentation: String): Board = {
    new Board(calculateBoardSize(stringRepresentation),
              calculateNumPlays(stringRepresentation))
  }

  /** Checking the size of the board and returning an Int */
  def calculateBoardSize(boardRepresentation: String): Int = {
    boardRepresentation.split("\n").head.count(_ == '+')

  }

  /** In this one, it tells you how many moves it can do with the board size and lines in the string*/
  def calculateNumPlays(boardStringView: String) = {
    val size = calculateBoardSize(boardStringView)
    val lines = boardStringView.split("\n")

    /** This gives the proper amount of horizontal lines to be made or rows to be made within this specific board size based on size*/
    def settingHorizLines(stringRow: Int, rowHorizontal: Int) = {
      val numOfSpaces = lines(stringRow).split("\\+")
      for {
        (gap, i) <- numOfSpaces.zipWithIndex
        if "===".equals(gap)
      } yield (i - 1 + ((size) * rowHorizontal), i + (size * rowHorizontal))
    }

    /** We will be setting the dividers to allow the vertical lines to be placed*/
    def settingVertLines(stringVerticalRow: Int, rowVertical: Int) = {
      for {
        (char, i) <- lines(stringVerticalRow).toCharArray.zipWithIndex
        if char == '|'
      } yield
        (((size + 1) * rowVertical) + (i / 4),
         ((size + 1) * rowVertical) + (i / 4) + (size + 1))
    }

    /** This is the number of plays and we put them into a list buffer or mutable list. This list will always
     *  be growing as the amount of plays will be decreasing as the game plays on.
     */
    val plays = new ListBuffer[(Int, Int)]()

    /** This for loop allows the correct math for the board to have right spaces between and then we add the particular lines 
     *  later from the calculated size
    */
    for (i <- 0 until size * 2 - 1) {
      if (i % 2 == 0) {
        plays ++= settingHorizLines(i, i / 2)
      } else {
        plays ++= settingVertLines(i, (i - 1) / 2)
      }
    }

    plays.result()
  }
}

// Every time an instance of board is created, it goes through these checks and creates these moves and fuction with teh number of plays and size.
class Board(val size: Int, val plays: List[(Int, Int)]) {
  require(size > 1, "Size of board must be greater than 1.")

// We calculate the area with the chosen size
  val area = size * size

  // This lists the possible moves in a list.
  // The final line with yield gives you an X,Y coordinate
  val possibleMoves = List.concat(
    for {
      i <- (0 to area).toList
      if (i + 1 < area && i % size != size - 1)
    } yield (i, i + 1),
    for {
      i <- (0 to area).toList
      if i + size < area
    } yield (i, i + size)
  )

  

  // puts all the availableMoves with a filter that only with plays we contained
  def availableMoves = {
    possibleMoves filterNot (plays contains)
  }
  // puts all the squares into a  list based off the different positions
  
  val possibleSquares = {

    for {
      i <- (0 to area).toList
      
      if i + 1 < area &&
        i % size != size - 1 &&
        i + size < area
//The yield gives the coordinates of all 4 lines that will make the square.
    } yield
      (((i, i + 1),
        (i + size, i + size + 1),
        (i, i + size),
        (i + 1, i + 1 + size),
        i))
  }
  // puts the the completed squares into a function while running play
  //(with a count function for winning)
  
  def completedSquares: Int = plays match {
    case Nil => 0
    case x :: xs => {

  //    println(possibleSquares)

// This counts the number of possible squares within the lines that are already placed that we can make for the computer to choose the best move.
      val sq = possibleSquares.count(
        squareCoords =>
          List(squareCoords._1, squareCoords._2, squareCoords._3, squareCoords._4)
            .forall(plays.contains))
      valueSquares(0, 0)
      sq

    }
  }
// This was my attempt in calculating the values and sums of the squares for the AI to use for its scoring function an successor function later on.
// A Work in progress
  def valueSquares(counter: Int = 0, sum: Int = 0): Int = {
    if (possibleSquares(counter) == possibleSquares(area / 2))
      return possibleSquares(counter)._5

    var sum2: Int = (possibleSquares(counter)._5)
    sum2 += valueSquares(counter + 1, sum2)
    sum2

  }

  // when we run the game these are the things that it checks for and ensures
  //that the lines are all fair and not already on board
  def play(line: (Int, Int)): Board = {
    require(!plays.contains(line),
            "You can't do that line because: " + line + " is already placed on the board")
    new Board(size, plays.::(line))
  }

  override def toString = {
    var out = ""

    // Making a horizontal lines
    def drawingHorizLines(start: Int, end: Int): String = {
      if (plays.contains((start, end))) "+==="
      else if (end % size == 0) "+"
      else "+   "
    }

    // making veritical lines with the bars
    def drawingVertLines(start: Int, end: Int): String = {
      val rowEnd = end % size == size - 1
      val exists = plays.contains((start, end))
      if (rowEnd) {
        if (exists) "|" else " "
      } else {
        if (exists) "|   " else "    "
      }
    }
    // with the row and going until the size of that we said
    // and then we add onto the output which is the String out
    var row = 0
    for (iter <- 0 until size * 2) { //-1
      if (iter % 2 == 0) {
        for (start <- row * size until (row + 1) * size)
          out += drawingHorizLines(start, start + 1)
      } else {
        for (start <- row * size until (row + 1) * size)
          out += drawingVertLines(start, start + size)
        row += 1
      }
      out += "\n"
    }
    out
  }
  // This allows the proper types to be implemente for Board Type
  def canEqual(anyType: Any): Boolean = anyType.isInstanceOf[Board]
// AN override of equals to allow a Board State to equal another to ensure we have not already visited this state.
  override def equals(anyType: Any): Boolean = anyType match {
    case caseBoard: Board =>
      (caseBoard canEqual this) &&
        size == caseBoard.size &&
        plays == caseBoard.plays
    case _ => false
  }
  // to ensure we all the visited states
  override def hashCode(): Int = {
    val state = Seq(size, plays)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 69420 * a + b)
  }
}

//////////////
//GameState//
///////////////

// tracking the game state withteh board and both players
class GameState(val board: Board, val player1: Player, val player2: Player) {

  // keeps track of completed squares, sometthing to be deleted after
  val completedSquares = board.completedSquares

  // a function to stay how many available moves there are .
  def availableMoves = board.availableMoves

  //val valueSqaure = board.valueSqaure

  // checks if the state we're in is terminal
  def isTerminal = board.availableMoves.isEmpty

  // look up what that heuristicValue is with the player score (heuristic)
  def heuristicValue = {
    if (player1.isCurrentPlayer) player1.score - player2.score
    else player2.score - player1.score
  }
  //making a move function
  def makeMove(move: (Int, Int)): GameState = {
    val newBoard = board.play(move)

    // the score to computed with the how many completed squares.
    val score = newBoard.completedSquares - completedSquares
    if (score == 0) {
      new GameState(board.play(move), player1.drawLine, player2.drawLine)
    } else {
      if (player1.isCurrentPlayer) {


        new GameState(board.play(move),
                      player1.completeBox(score),
                      player2.completeBox(0))
      } else {

        new GameState(board.play(move),
                      player1.completeBox(0),
                      player2.completeBox(score))

      }
    }
  }
  // changes player control if it's not player1 turn
  def currentPlayer = {
    if (player1.isCurrentPlayer) player1 else player2
  }
}

///////
//////Player///
//////////////////


//   a GameStyle (either Human or AI), their score, and whether or not it's their turn
class Player(val GameStyle: GameStyle,
             val score: Int,
             val isCurrentPlayer: Boolean) {

  // choose the person who' going to play next with their specific characteristic
  def pickPlay(state: GameState): (Int, Int) = {
    GameStyle.play(state)
  }

  // uses the palyer function and just adds their score which we need to implement
  def completeBox(score: Int): Player =
    new Player(GameStyle, this.score + score, !isCurrentPlayer)

  //Drawing the line and switching turns
  def drawLine: Player = new Player(GameStyle, score, !isCurrentPlayer)

  // prints out which player
  override def toString = GameStyle.toString
}

/////
//GameRound/////////
///////


class GameRound(val rounds: Int,
                val GameStyle1: GameStyle,
                val GameStyle2: GameStyle) {

  var player1score = 0
  var player2score = 0
  var winState: String = " "

  def play() = {
    for (_ <- 1 to rounds) {
      val finalState = new SucessorFunction(GameStyle1, GameStyle2).play

      player1score += finalState.player1.score
      player2score += finalState.player2.score
    }
    if (player1score > player2score) {
      winState + "Human Wins"
    } else if (player1score < player2score) {
      winState + "AI Wins"
    } else {
      winState + "It's a draw"
    }

    println(formatScores)
  }

  private def formatScores = {
    val out = ("     " + GameStyle1 + "   | " + GameStyle2 + "  \n" +
      "  ===========|=========== \n" +
      "Score:  " + player1score + "        | " + player2score + " \n" + "\n" + winState)
    out
  }
}

/////  (HUMAN)
////////

class Human extends GameStyle {

  def play(state: GameState): (Int, Int) = {

    println(state.availableMoves)

    println(" ")
    println(
      "Your turn! Where would you like to go? \t" +
        "Choose a column")
    val input: Int = StdIn.readInt().asInstanceOf[Int]
    println("Choose a row")
    val input2: Int = StdIn.readInt().asInstanceOf[Int]
    val mymove = (input, input2)
    mymove

  }

  override def toString = "HUMAN"
}

/////
//MiniMax//
////

class Minimax extends GameStyle {

  val maxDepth = DotsNBoxesGame.plyNum
// The max depth which is the PlyNum specified

// For minimax, it will need to return a pair of Int or Coordinates to continue playing
  def play(state: GameState): (Int, Int) = {
    val moveScores = {
      for { move <- state.availableMoves } yield
        move -> minimax(state.makeMove(move))
    }
    val bestMove = moveScores.minBy(_._2)._1
    bestMove
  }

  private def minimax(state: GameState): Int = {
    minimize(state, maxDepth, Integer.MIN_VALUE, Integer.MAX_VALUE)
  }

  private def minimize(state: GameState,
                       depth: Int,
                       alpha: Int,
                       beta: Int): Int = {
    if (state.isTerminal || depth == 0) return state.heuristicValue
    var newBeta = beta
    // for all the moves it will go through each move and make a move with the sucessor fucnction and take a min to see which is the minimum value that it can
                          // assign to beta.
    state.availableMoves.foreach(move => {
      val newState = state.makeMove(move)
      newBeta = math.min(beta, maximize(newState, depth - 1, alpha, newBeta))
      //if the alpha becomes bigger than the smallest beta, then we use that value for alpha above the tree
      if (alpha >= newBeta) return alpha
    })
    newBeta
  }

  private def maximize(state: GameState,
                       depth: Int,
                       alpha: Int,
                       beta: Int): Int = {
    if (state.isTerminal || depth == 0) return state.heuristicValue
    var newAlpha = alpha
    // Same kind of thing where we look through all the moves anad choose a maximum instead and percolates a beta move up tree after finding 
                        // the max alpha val
    state.availableMoves.foreach(move => {
      val newState = state.makeMove(move)
      newAlpha =
        math.max(newAlpha, minimize(newState, depth - 1, newAlpha, beta))
      if (newAlpha >= beta) return beta
    })
    newAlpha
  }

  override def toString = "MINIMAX"
}

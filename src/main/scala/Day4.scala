import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

val numbers = List[Int](90, 4, 2, 96, 46, 1, 62, 97, 3, 52, 7, 35, 50, 28, 31,
  37, 74, 26, 59, 53, 82, 47, 83, 80, 19, 40, 68, 95, 34, 55, 54, 73, 12, 78,
  30, 63, 57, 93, 72, 77, 56, 91, 23, 67, 64, 79, 85, 84, 76, 10, 58, 0, 29, 13,
  94, 20, 32, 25, 11, 38, 89, 21, 98, 92, 42, 27, 14, 99, 24, 75, 86, 51, 22,
  48, 9, 33, 49, 18, 70, 8, 87, 61, 39, 16, 66, 71, 5, 69, 15, 43, 88, 45, 6,
  81, 60, 36, 44, 17, 41, 65)

def parseInputs(rawInput: List[String]): List[List[List[BoardValue]]] = {
  var boards = ListBuffer[List[List[BoardValue]]]()

  var currentBoard = ListBuffer[List[BoardValue]]()

  rawInput.foreach(row => {

    if (row.length == 0) {
      boards += currentBoard.toList
      currentBoard = ListBuffer[List[BoardValue]]()
    } else {

      val elements = row
        .split("\\s+")
        .map(_.toIntOption)
        .flatten
        .map(value => BoardValue(value, false))
        .toList

      currentBoard += elements

    }
  })

  boards.toList
}

case class BoardValue(value: Int, marked: Boolean)

def markNumber(
    board: List[List[BoardValue]],
    number: Int
): List[List[BoardValue]] = {

  board.map(row =>
    row.map(boardValue => {
      if (boardValue.value == number) {
        BoardValue(boardValue.value, true)
      } else {
        boardValue
      }
    })
  )

}

def checkBoard(board: List[List[BoardValue]]): Boolean = {
  var index = 0

  var hasWon = false

  for (index <- 0 to 4) {

    val isWinnerRow = board(index).forall(value => value.marked)

    val isWinnerCol = board.forall(row => row(index).marked)

    if (isWinnerCol || isWinnerRow) {
      hasWon = true
    }
  }

  hasWon

}

def calculateScore(board: List[List[BoardValue]], number: Int): Int = {

  val boardScore = board
    .map(row =>
      row
        .filter(boardValue => boardValue.marked == false)
        .map(boardValue => boardValue.value)
    )
    .flatten
    .sum

  boardScore * number

}

def day4Part1(fileLocation: String): Unit = {
  val input = readFile(fileLocation)

  var parsedInput = parseInputs(input)

  var number = 0

  breakable {
    for (number <- numbers) {
      println(number)

      parsedInput = parsedInput.map(board => markNumber(board, number))

      parsedInput.foreach(board => {
        val hasWon = checkBoard(board)

        if (hasWon) {

          val score = calculateScore(board, number)

          println(score)

          break
        }
      })

    }
  }

}

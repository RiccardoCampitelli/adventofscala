import scala.collection.mutable.ListBuffer

def day1Part1(fileLocation: String): Unit = {

  val input = readFile(fileLocation).map(row => row.toInt);

  var index = 0

  var current = input(0)

  var count = 0

  for (index <- 1 to input.length - 1) {

    val next = input(index)

    if (next > current) {
      count = count + 1
    }

    current = next
  }

  println(count)

}

def day1Part2(fileLocation: String): Unit = {
  val input = readFile(fileLocation).map(row => row.toInt);

  var count = 1
  var total = 0
  var index = 0

  var groupedInput = ListBuffer[Int]()

  for (index <- 0 to input.length - 3) {

    var current = 0

    current = current + input.lift(index).getOrElse(0)
    current = current + input.lift(index + 1).getOrElse(0)
    current = current + input.lift(index + 2).getOrElse(0)

    groupedInput += current
  }

  var current = groupedInput(0)

  var result = 0
  index = 1

  for (index <- 1 to groupedInput.length - 1) {

    val next = groupedInput(index)

    if (next > current) {
      result = result + 1
    }

    current = next
  }

  println(result)
}

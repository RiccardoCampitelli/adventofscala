def day2Part1(fileLocation: String): Unit = {
  val input = readFile(fileLocation).map(parseRow)

  var horizontal = 0
  var depth = 0

  input.foreach(_ match {
    case ("forward", distance) => horizontal = horizontal + distance
    case ("down", distance)    => depth = depth + distance
    case ("up", distance)      => depth = depth - distance
  })

  println(horizontal * depth)
}

def parseRow(row: String): (String, Int) = {

  val splitRow = row.split(" ")

  val direction = splitRow(0)

  val distance = splitRow(1).toInt

  (direction, distance)
}

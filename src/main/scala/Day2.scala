def day2Part1(fileLocation: String): Unit = {
  val input = readFile(fileLocation).map(parseRow)

  var horizontal = 0
  var depth = 0
  var aim = 0

  input.foreach(_ match {
    case ("forward", distance) => {
      horizontal = horizontal + distance
      val depthToAdd = aim * distance
      depth = depth + depthToAdd
    }
    case ("down", distance) => {
      aim = aim + distance
    }
    case ("up", distance) => {
      aim = aim - distance
    }
  })

  println(horizontal * depth)
}

def parseRow(row: String): (String, Int) = {

  val splitRow = row.split(" ")

  val direction = splitRow(0)

  val distance = splitRow(1).toInt

  (direction, distance)
}

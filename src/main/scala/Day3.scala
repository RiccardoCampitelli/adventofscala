def day3Part1(fileLocation: String): Unit = {
  val input = readFile(fileLocation)

  var frequency = List[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  input.foreach(row => {
    var index = 0

    for (index <- 0 to row.length - 1) {

      val char = row(index)

      if (char == '1') {
        val current = frequency(index)

        frequency = frequency.updated(index, current + 1)
      }
    }
  })

  val limit = input.length / 2;

  val gamma = frequency.map(el => if (el > limit) 1 else 0)

  val epsilon = gamma.map(_ match {
    case 1 => 0
    case 0 => 1
  })

  val gammaValue = Integer.parseInt(gamma.map(_.toString).mkString, 2)

  val epsilonValue = Integer.parseInt(epsilon.map(_.toString).mkString, 2)

  println(gammaValue * epsilonValue)

}

def day3Part2(fileLocation: String): Unit = {

  val input = readFile(fileLocation)

  var oxygenValues = input.map(identity)
  var scrubberValues = input.map(identity)

  var rowIndex = 0

  var currentRow = ""

  var oxygenRating = 0
  var c02Rating = 0

  for (rowIndex <- 0 to 11) {

    var frequency = 0

    for (currentRow <- oxygenValues) {

      val currentChar = currentRow.charAt(rowIndex)

      if (currentChar == '1') {
        frequency = frequency + 1
      }
    }

    var halfOxygen = oxygenValues.length.toDouble / 2

    var toFilter = if (frequency >= halfOxygen) '1' else '0'

    oxygenValues = oxygenValues.filter(el => el.charAt(rowIndex) == toFilter);

    if (oxygenValues.length == 1) {
      println(oxygenValues(0))

      oxygenRating = Integer.parseInt(oxygenValues(0), 2)
    }

    frequency = 0

    for (currentRow <- scrubberValues) {
      val currentChar = currentRow.charAt(rowIndex)

      if (currentChar == '1') {
        frequency = frequency + 1
      }
    }

    var halfScrubber = scrubberValues.length.toDouble / 2

    toFilter = if (frequency >= halfScrubber) '0' else '1'

    scrubberValues =
      scrubberValues.filter(el => el.charAt(rowIndex) == toFilter)

    if (scrubberValues.length == 1) {
      println(scrubberValues(0))

      c02Rating = Integer.parseInt(scrubberValues(0), 2)
    }

  }

  println(c02Rating)
  println(oxygenRating)

  println(c02Rating * oxygenRating)

}

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

  val epsilon = gamma.map(el =>
    el match {
      case 1 => 0
      case 0 => 1
    }
  )

  val gammaValue = Integer.parseInt(gamma.map(_.toString).mkString, 2)

  val epsilonValue = Integer.parseInt(epsilon.map(_.toString).mkString, 2)

  
  println(gammaValue * epsilonValue)

}

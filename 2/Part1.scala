import scala.io.Source

object Part1 {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line).toSeq


    val validPwCounts= lines.map(line => {
      val elements = line.split(" ")
      val minMaxOccurences = elements(0).split("-")
      val validLetter = elements(1)(0)
      val password = elements(2)
   
      println("==============")

      isPWValid(
        password,
        validLetter,
        minMaxOccurences(0).toInt,
        minMaxOccurences(1).toInt
      )
    }).filter(identity)
    .size

    println(validPwCounts)

  }

  def isPWValid(
    password: String,
    validLetter: Char,
    minOccurences: Int,
    maxOccurences: Int
  ): Boolean = {
    val occurences = password
      .filter(cha => cha == validLetter)
      .size

    if(occurences <= maxOccurences && occurences >= minOccurences) {
      println(password)
      println(occurences)
      println(validLetter + " " + minOccurences + " " + maxOccurences)
      true
    }
    else {
      false
    }
    
  }
}

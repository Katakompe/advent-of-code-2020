import scala.io.Source

object Part2 {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line).toSeq


    val validPwCounts= lines.map(line => {
      val elements = line.split(" ")
      val positions = elements(0).split("-")
      val validLetter = elements(1)(0)
      val password = elements(2)
      isPWValid(
        password,
        validLetter,
        positions(0).toInt-1,
        positions(1).toInt-1
      )
    }).filter(identity)
    .size

    println(validPwCounts)

  }

  def isPWValid(
    password: String,
    validLetter: Char,
    pos1: Int,
    pos2: Int
  ): Boolean = {
    (password(pos1) == validLetter && password(pos2) != validLetter) || (password(pos1) != validLetter && password(pos2) == validLetter)
  }
}

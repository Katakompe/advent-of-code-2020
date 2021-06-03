import scala.io.Source
import java.util.concurrent.TimeUnit


object Part1 {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line).toSeq

    val t1 = System.nanoTime


    println(lines.size)
    val validPwCounts= lines.map(line => {
      val elements = line.split(" ")
      val minMaxOccurences = elements(0).split("-")
      val validLetter = elements(1)(0)
      val password = elements(2)
   

      isPWValid(
        password,
        validLetter,
        minMaxOccurences(0).toInt,
        minMaxOccurences(1).toInt
      )
    }).filter(identity)
    .size

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
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
      
      true
    }
    else {
      false
    }
    
  }
}

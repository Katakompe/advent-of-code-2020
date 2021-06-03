import scala.io.Source
import java.util.concurrent.TimeUnit

object Part2 {

  val binaryMap: Map[Char, Char] =
    Map('F' -> '0', 'B' -> '1', 'R' -> '1', 'L' -> '0')

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    
    val existingSeats = lines
    .map(line => Integer.parseInt(line.map(char => binaryMap.get(char).get).mkString, 2))
   

    val allSeats: Seq[Int] = (0 to Math.pow(2,10).toInt).toSeq
    
    val missingSeats = allSeats.diff(existingSeats)
    
    println(missingSeats)






  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }
}

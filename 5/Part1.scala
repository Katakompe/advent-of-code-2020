import scala.io.Source
import java.util.concurrent.TimeUnit

object Part1 {

  val binaryMap: Map[Char, Char] =
    Map('F' -> '0', 'B' -> '1', 'R' -> '1', 'L' -> '0')

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    
    val maxSeat = lines
    .map(line => Integer.parseInt(line.map(char => binaryMap.get(char).get).mkString, 2))
    .max

    
    println(maxSeat)






  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }
}

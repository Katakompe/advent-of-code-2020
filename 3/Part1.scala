import scala.io.Source
import java.util.concurrent.TimeUnit

object Part1 {
  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")

    val t1 = System.nanoTime

    val treeHits:Int = lines
      .map(line => line.replace(" ", ""))
      .zipWithIndex
      .map(line => (line._2*3, line._1.size, line._1))
      .map(mapToTreeHits)
      .sum
      
    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(treeHits)

  }

  val mapToTreeHits: ((Int, Int, String)) => Int = ((i: Int, lineLenth: Int, line: String) => {
    if (line(i % lineLenth) == '#') {
      1
    } else if(line(i%lineLenth) == '.') {
      0
    } else throw new IllegalStateException("Encountered different symbol: " + line(i%lineLenth))
  }).tupled

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }
}

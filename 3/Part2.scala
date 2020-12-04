import scala.io.Source
import java.util.concurrent.TimeUnit

object Part2 {

  val slopes: List[Tuple2[Double, Int]] = List((1, 1), (3, 1), (5, 1), (7, 1), (0.5, 2))

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    val t1 = System.nanoTime
    val treeHits: List[Long] = lines
      .map(line => line.replace(" ", ""))
      .zipWithIndex
      .map(line => (line._2, line._1.size, line._1))
      .map(tup => mapToTreeHits(tup._1, tup._2, tup._3))
      .reduce((hitList1, hitList2) =>
        hitList1.zip(hitList2).map { case (x, y) => x + y }
      ).map(_.toLong)

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(treeHits.product)
    

  }

  def mapToTreeHits(i: Int, lineLenth: Int, line: String) = {
    for (slope <- slopes)
      yield
        if (i % slope._2 == 0 && line((slope._1 * i).toInt % lineLenth) == '#') 1
        else 0

  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }
}

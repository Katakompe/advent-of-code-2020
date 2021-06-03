import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt").map(_.toLong).sorted

    val t1 = System.nanoTime

    val originalLines: Seq[Long] = Seq(0L) ++ lines

    val linesShifted: Seq[Long] = lines ++ Seq(lines.last+3L)

    println(originalLines)
    println(linesShifted)

    val countmaps = originalLines.zip(linesShifted).map(zipping => math.abs(zipping._1-zipping._2)).groupBy(identity(_))
      .mapValues(_.size)

    println(countmaps.toSeq)

    val result = countmaps.get(1L).get * countmaps.get(3L).get


    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

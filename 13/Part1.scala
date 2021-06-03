import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {


  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    val t1 = System.nanoTime

    val myTime = lines(0).toLong
    val depTimes = lines(1)
    .split(',')
    .filter(!_.contains('x'))
    .map(_.toLong)
    .map(value => (value, value - myTime % value ))
    .sortBy(_._2)
    depTimes.foreach(println(_))
    val result = depTimes(0)._1*depTimes(0)._2

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

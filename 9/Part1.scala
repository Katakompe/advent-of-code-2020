import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt").map(_.toLong)

    val t1 = System.nanoTime

    val invalidNumber = (for (
      (value, i) <- lines.zipWithIndex;
      if (i > 24 && !isSumOfLast(value, i, lines, 25))
    ) yield value)(0)

    val result = findSumList(invalidNumber, lines)

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

  def findSumList(number: Long, lines: Seq[Long]): Long = {
    var window: List[Long] = List()
    var i = 0
    while (i < lines.size) {
      while (window.sum < number) {
        window = window :+ lines(i)
        i+=1
        if (window.sum == number) {
          return window.min + window.max
        }
      }
      while (window.sum > number) {
        window = window.tail
         if (window.sum == number) {
          return window.min + window.max
        }
      }
    }
    -1L
  }

  def isSumOfLast(
    value: Long,
    i: Int,
    lines: Seq[Long],
    lastN: Int
  ): Boolean = {
    var res = false
    for (
      summand1 <- lines.slice((i - lastN), i);
      summand2 <- lines.slice((i - lastN), i);
      if (summand1 + summand2 == value)
    ) {
      res = true
    }
    res
  }

}

import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object Part1 {

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt").toArray

    val t1 = System.nanoTime


    var change = true
    while (change) {
      val linesOld = lines.clone()
      change = false
      for (i <- (0.to(lines.size-1))) {
        for (j <- (0.to(lines(0).size-1))) {
          val seat = linesOld(i)(j)
          if (seat == 'L' && surroundingsEmpty(i, j, linesOld)) {
            val strBuilder: StringBuilder = new StringBuilder()
            strBuilder.append(lines(i))
            strBuilder.setCharAt(j, '#')
            lines(i) = strBuilder.toString()
            change = true
          }
          else if (seat == '#' && fourSeatsOccupied(i, j, linesOld)) {
            val strBuilder: StringBuilder = new StringBuilder()
            strBuilder.append(lines(i))
            strBuilder.setCharAt(j, 'L')
            lines(i) = strBuilder.toString()
            change = true
          }
        }
      }
    }
    val result = lines.map(line => line.filter(char => char=='#').size).sum


    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def surroundingsEmpty(i: Int, j: Int, linesOld: Seq[String]): Boolean = {
    val surrounds: Seq[Char] = for (
      row <- (i - 1).to(i + 1);
      col <- (j - 1).to(j + 1);
      if (row != i || col != j) && row>=0 && col>=0 && row<linesOld.size && col<linesOld(0).size) yield linesOld(row)(col)
    surrounds.filter(el => el == '#').size == 0
  }

  def fourSeatsOccupied(i: Int, j: Int, linesOld: Seq[String]): Boolean = {
    val surrounds: Seq[Char] = for (
      row <- (i - 1).to(i + 1);
      col <- (j - 1).to(j + 1);
      if (row != i || col != j) && row>=0 && col>=0 && row<linesOld.size && col<linesOld(0).size) yield linesOld(row)(col)
    surrounds.filter(el => el == '#').size > 3
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

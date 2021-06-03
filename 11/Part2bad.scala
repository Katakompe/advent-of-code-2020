import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object Part2 {

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
      println("new iteration")
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
      row <- (0).to(linesOld.size-1);
      col <- (0).to(linesOld(0).size-1);
      if canBeSeenFrom(i,j, linesOld, row, col)
     ) yield linesOld(row)(col)

    surrounds.filter(el => el == '#').size > 4
  }

  def canBeSeenFrom(i: Int,j: Int, linesOld: Seq[String], row: Int, col:Int): Boolean = {
    if((i==row && col == j) || (linesOld(row)(col)=='.') || (linesOld(row)(col)=='L')){
      return false
    } else if (i==row && col != j) {
      if(!linesOld(row).substring(math.min(col,j)+1, math.max(col, j)).contains('#') && !linesOld(row).substring(math.min(col,j)+1, math.max(col, j)).contains('L')) {
        return true
      } else {
        return false
      }
    } else if(i!= row && col == j) {
      if(!(linesOld.transpose(_.toSeq))(col).mkString("").substring(math.min(row,i)+1, math.max(row, i)).contains('#') && !(linesOld.transpose(_.toSeq))(col).mkString("").substring(math.min(row,i)+1, math.max(row, i)).contains('L')){
        return true
      } else {
        return false
      }
    } else if(i-j == row-col) {
      val diag = for(
        k <- linesOld.indices; 
        l<-linesOld(0).indices;
        if(k-l==row-col)
        ) yield linesOld(k)(l)
      val diagString = diag.mkString("")
      val srcPointIndex = if(row-col < 0) i else j
      val dstPointIndex = if(row-col < 0) row else col
      val lowerIndex = math.min(srcPointIndex, dstPointIndex)
      val upperIndex = math.max(srcPointIndex, dstPointIndex)

      if (math.abs(upperIndex-lowerIndex) == 1) {
        return true
      }

      if(!diagString.substring(lowerIndex+1, upperIndex).contains('#') && !diagString.substring(lowerIndex+1, upperIndex).contains('L')) {
        return true
      } else{
        return false
      }
    } else {
      return false
    }
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

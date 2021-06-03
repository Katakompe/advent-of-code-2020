import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object Part2 {

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input2.txt").toArray
  

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
      row <- (0).to(linesOld.size-1);
      col <- (0).to(linesOld(0).size-1);
      if canBeSeenFrom(i,j, linesOld, row, col)
     ) yield linesOld(row)(col)
    surrounds.size == 0
  }

  def fourSeatsOccupied(i: Int, j: Int, linesOld: Seq[String]): Boolean = {
    val surrounds: Seq[Tuple3[Int, Int,Char]] = for (
      row <- (0).to(linesOld.size-1);
      col <- (0).to(linesOld(0).size-1);
      if canBeSeenFrom(i,j, linesOld, row, col)
     ) yield (row,col, linesOld(row)(col))
    surrounds.size > 4
  }

  def canBeSeenFrom(i: Int,j: Int, linesOld: Seq[String], row: Int, col:Int): Boolean = {
    if((i==row && col == j) || (linesOld(row)(col)=='.') || (linesOld(row)(col)=='L')){
      return false
    } else if (i==row && col != j) {
      val from = math.min(col,j)
      val to = math.max(col,j)
      var it = from+1
      var size = linesOld(i).size
      while(it<size) {
        if(it==to){
          return true
        }
        if(linesOld(i)(it) != '.') {
          return false
        }
        it+=1
      }
      return false
    } else if(i!= row && col == j) {
      val from = math.min(row,i)
      val to = math.max(row,i)
      var it = from+1
      var size = linesOld.size
      while(it<size) {
        if(it==to){
          return true
        }
        if(linesOld(it)(j) != '.') {
          return false
        }
        it+=1
      }
      return false
    } else if(i-j == row-col) {
      val from = if(i+j<row+col) (i,j) else (row,col)
      val to = if(i+j<row+col) (row,col) else (i,j)
      var it = (from._1+1, from._2+1)
      while(it._1<linesOld.size && it._2<linesOld(i).size) {
        if(it==to){
          return true
        }
        if(linesOld(it._1)(it._2) != '.') {
          return false
        }
        it = (it._1+1, it._2+1)
      }
      return false
    } else if(i+j == row+col) {
      val from = if(j<col) (i,j) else (row,col)
      val to = if(j>col) (i,j) else (row,col)
      var it = (from._1-1, from._2+1)
      while(it._1>(-1) && it._2< linesOld(0).size) {
        if(it==to){
          return true
        }
        if(linesOld(it._1)(it._2) != '.') {
          return false
        }
        it = (it._1-1, it._2+1)
      }
      return false
    } else {
      return false
    }
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

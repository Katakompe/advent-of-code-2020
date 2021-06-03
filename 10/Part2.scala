import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map}
import scala.collection.mutable

object Part2 {

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt").map(_.toLong).sorted

    val t1 = System.nanoTime

    val originalLines: Seq[Long] = Seq(0L) ++ lines ++ Seq(lines.last + 3L)

    var splits :ArrayBuffer[Seq[Long]] = ArrayBuffer()
    var startIndex = 0
    var endindex = 0
    for((value, i)<- originalLines.zipWithIndex) {
      if(originalLines.size > i+1 && originalLines(i+1)-value == 3) {
        endindex = i+1
        splits.+=(originalLines.slice(startIndex, endindex))
        startIndex = endindex
      }
    }

    val result = splits.map(split => {
      val sum = buildTree(split(0), split, 0)
      println(sum)
      sum
    }).reduce((a,b)=> a*b)


    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

  def buildTree(value: Long, originalLines: Seq[Long], index: Int):Long = {
    if (value != originalLines.max) {
      var sum = 0L
      for (i <- (1 to (3))) {
        if (originalLines.size > index+i && originalLines(index + i) - value < 4) {
          val child= originalLines(index + i)
          sum += buildTree(child, originalLines, index+i)
        }
      }
      sum
    } else {
      1L
    }

  }

  case class Tree(val value: Long, var children: Seq[Tree]) {
    def addChild(in: Tree): Tree = {
      children = children ++ Seq(in)
      in
    }
  }

}

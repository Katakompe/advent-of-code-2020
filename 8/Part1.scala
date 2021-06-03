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
    val commands = lines
      .map(line => line.split(" "))
      .map(parts => (parts(0), parts(1).toInt))

    val t1 = System.nanoTime

    var visited: Set[Int] = Set()
    var i = 0
    var acc = 0

    while (!isInfiniteLoop(i, visited)) {
      visited = visited + i
      commands(i) match {
        case ("acc", incr) => acc+=incr
        case ("jmp", incr) => i+=incr-1
        case ("nop", _) => 
      }
      i+=1
    }

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println("Result: " + acc)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

  def isInfiniteLoop(i: Int, visited: Set[Int]) = visited.contains(i)
}

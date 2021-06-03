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
    val originalCommands = lines
      .map(line => line.split(" "))
      .map(parts => (parts(0), parts(1).toInt))

    val t1 = System.nanoTime
    
    var visited: Set[Int] = Set()

    var i = 0
    var acc = 0

    var commands = originalCommands.clone()
    //Indicates how many jmps/nops to skip until one is changed
    var changedJmp = 0

    var skipsLeft = changedJmp
    while (i < commands.size) {
      if (isInfiniteLoop(i, visited)) {
        println("Inf loop found at i="  + i + "and changedJmp=" + changedJmp)
        changedJmp += 1
        skipsLeft = changedJmp
        commands=originalCommands.clone()
        i = 0
        acc = 0
        visited = Set()
      } else {
        visited = visited + i
        commands(i) match {
          case ("acc", incr) => acc += incr
          case ("jmp", incr) => {
            if (skipsLeft == 0) {
              commands(i) = ("nop", incr)
              skipsLeft = -1
            } else {
              skipsLeft -= 1
              i += incr - 1
            }
          }
          case ("nop", incr) => {
            if (skipsLeft == 0) {
              commands(i) = ("jmp", incr)
              skipsLeft = -1
              i += incr - 1
            } else {
              skipsLeft -= 1
            }
          }
        }
      }
      i += 1
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

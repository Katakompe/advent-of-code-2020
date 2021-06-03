import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {

  val rotationList = List('N', 'E', 'S', 'W')

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    val t1 = System.nanoTime

    val commands: Seq[Tuple2[Char, Int]] = lines.map(line => (line.head, line.tail.toInt))
    val compassCommands = commands.filter(com=> Seq('N', 'S', 'W', 'E').contains(com._1))
    val moveCommands = commands.diff(compassCommands)
    
    var direction = 'E'
    var result = 0

    for(com <- moveCommands) {
      if(com._1 == 'F'){
        result = result + addMovement(direction, com._2)
      } else {
        direction = rotateShip(direction, com)
      }
    }

    result = result + compassCommands
    .map(com => addMovement(com._1, com._2))
    .reduce(_+_)

    result = math.abs(result)

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def addMovement(dir: Char, len: Int): Int = {
    if(Seq('N', 'W').contains(dir)) {
      -len
    } else {
      len
    }
  }

  def rotateShip(currentDir: Char, command: Tuple2[Char, Int]): Char = {
    val newDir = if(command._1=='R') (rotationList.indexOf(currentDir) + command._2/90)%4 else (((rotationList.indexOf(currentDir) - command._2/90)%4) +4)%4
    rotationList(newDir)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

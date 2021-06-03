import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import java.io.FileOutputStream
import java.io.PrintStream
object Part2 {

  val rotationList = List('N', 'E', 'S', 'W') 
  var wayPoint = (10,-1)

  implicit class TuppleAdd(t: (Int, Int)) {
    def +(p: (Int, Int)) = (p._1 + t._1, p._2 + t._2)
  }


  def main(args: Array[String]): Unit = {
   // System.setOut(new PrintStream(new FileOutputStream("test.log")))

    val lines = parseInput("input.txt")
    val t1 = System.nanoTime

    val commands: Seq[Tuple2[Char, Int]] = lines.map(line => (line.head, line.tail.toInt))
    
    var result = (0,0)

    for(com <- commands) {
      com match {
        case ('F', len:Int) => result = result + addMovement(len)
        case ('R', deg:Int) => rotateShip('R', deg)
        case ('L', deg:Int) => rotateShip('L', deg)
        case (dir:Char, len:Int) => moveWayPoint(dir, len)
      }
      print("result " + result + ", \t")
      println("waypoint", wayPoint)
    }

     

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(math.abs(result._1) + math.abs(result._2))
  }

  def addMovement(len: Int): Tuple2[Int, Int] = {
    (wayPoint._1*len, wayPoint._2*len)
  }

  def rotateShip(rotation: Char, deg: Int) = {

    val actualDegrees = if(rotation == 'L') 360-deg else deg
    val rad = Math.toRadians(actualDegrees)
    wayPoint = ((wayPoint._1 * Math.cos(rad) - wayPoint._2 * Math.sin(rad)).round.toInt, (wayPoint._1*Math.sin(rad) + wayPoint._2 * Math.cos(rad)).round.toInt)
  }

  def moveWayPoint(dir: Char, len: Int) = {
    wayPoint = dir match {
      case 'N' => (wayPoint._1, wayPoint._2 - len)
      case 'S' => (wayPoint._1, wayPoint._2 + len)
      case 'W' => (wayPoint._1 - len, wayPoint._2)
      case 'E' => (wayPoint._1 + len, wayPoint._2)
    }
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

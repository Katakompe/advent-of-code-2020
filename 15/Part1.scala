import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {


  def main(args: Array[String]): Unit = {
    val input = List(6,19,0,5,7,13,1)
    
    //val input = List(3, 1, 2)
    val t1 = System.nanoTime
    
    var valueMap: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map()
    var lastSpoken: Int = -1


    for (i<- 1 to input.size) {
      val value = input(i-1)
      valueMap.update(value, i)
      lastSpoken = value
    }
    for(i<- input.size+1 to 30000000) {
      val value = lastSpoken
      if(valueMap.contains(value) && valueMap.get(value).get != i-1) {
        val turnDiff = (i-1) - valueMap.get(value).get
        valueMap.update(value, i-1)
        lastSpoken = turnDiff
      } else {
        valueMap.update(value, i-1)
        lastSpoken = 0
      }
    }

    val result = lastSpoken

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }


  def calculateValue(mask: String, value: String) = {
    java.lang.Long.parseLong(mask.zip(value).map{
      case ('X', v) => v
      case (m , v) => m
    }.mkString(""), 2)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

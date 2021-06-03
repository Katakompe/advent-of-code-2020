import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
object Part2 {


  def gcd(a: BigInt, b: BigInt):BigInt=if (b==0) a.abs else gcd(b, a%b)

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a,b) => (a/gcd(a,b))*b)

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")
    val t1 = System.nanoTime

    val depTimes = lines(1)
    .split(',')
    .zipWithIndex
    .filter(!_._1.contains('x'))
    .map(value => (BigInt(value._1), value._2))
    //.sortBy(_._1)

    var startValue:BigInt = 0
    var jumpSize = depTimes(0)._1
    var hitList: ArrayBuffer[Tuple2[BigInt, Int]] = ArrayBuffer()
    while(hitList.size!=depTimes.size) {
      
      startValue += jumpSize
      hitList = conditionsFullfilled(depTimes, startValue)
      jumpSize = hitList.map(value => value._1).reduce(_*_)
      println("aliginingbusses: ", hitList)
    }
  
  
    val result = startValue
    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def conditionsFullfilled(values: Seq[Tuple2[BigInt, Int]], kgv: BigInt): ArrayBuffer[Tuple2[BigInt, Int]] = {
    var returnList: ArrayBuffer[Tuple2[BigInt, Int]] = ArrayBuffer()
    for ((value, offset)<-values) {
      println("kgv, value, offset", kgv, value, offset)
      if((kgv+offset)%value == 0) {
        returnList.+=((value,offset))      
      } else {
        return returnList
      }
    }
    return returnList
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

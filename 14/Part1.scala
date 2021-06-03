import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {


  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt").mkString("\n")
    val t1 = System.nanoTime

    var valueMap: scala.collection.mutable.Map[String, Long] = scala.collection.mutable.Map()

    lines.split("mask = ").map(task=> {
      val inputs = task.split("\n")
      val mask = inputs.head
      val memJobTuples = inputs.tail.map(memjob=> {
        val binaryValue: String = memjob.split("=")(1).trim().toLong.toBinaryString
        val memPart: String = memjob.split("=")(0).trim()
        val memAdress: String = memPart.substring(4, memPart.size-1)
        
        val binaryValuePadded = binaryValue.reverse.padTo(mask.size, '0').reverse

        (memAdress, binaryValuePadded)
      })
      (mask, memJobTuples)
    }).foreach(job=> {
      val mask = job._1
      job._2.foreach(memtask => {
        val newVal = calculateValue(mask, memtask._2)
        valueMap.update(memtask._1, 
        newVal)
      })

    })



    val result = valueMap.values.sum

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

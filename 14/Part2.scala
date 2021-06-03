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
        val value: Long = memjob.split("=")(1).trim().toLong
        val memPart: String = memjob.split("=")(0).trim()
        val memAdress: String = memPart.substring(4, memPart.size-1).toInt.toBinaryString.reverse.padTo(mask.size, '0').reverse

        

        (memAdress, value)
      })
      (mask, memJobTuples)
    }).foreach(job=> {
      val mask = job._1
      job._2.foreach(memtask => {
        val keys = calculateMemAdress(mask, memtask._1)
        keys.foreach(key => valueMap.update(key.toString(), memtask._2))
      })

    })



    val result = valueMap.values.sum

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }


  def calculateMemAdress(mask: String, value: String): Seq[Long] = {
    
    val modifiedvalue = mask.zip(value).map{
      case ('X', v) => 'X'
      case ('0' , v) => v
      case ('1', v) => '1'
    }
    val noOfX = modifiedvalue.filter(_=='X').size
    val insertionLists: Seq[List[Char]] = for(i<- 0 to((Math.pow(2,noOfX)).round.toInt-1)) yield i.toBinaryString.reverse.padTo(noOfX,'0').reverse.toList
    insertionLists.map(list => {
      val srcvalue = modifiedvalue.toArray.clone()
      for(n<- list) {
        srcvalue(srcvalue.indexOf('X')) = n
      }
      java.lang.Long.parseLong(srcvalue.mkString(""), 2)
    })

  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }


}

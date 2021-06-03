import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Part1 {

  def main(args: Array[String]): Unit = {
    //val input = parseInput("input.txt")

    val t1 = System.nanoTime

    job()

    val result = 0

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

  def job() = {
    val regex = "(\\d)".r
    val regexOperator = "(\\+|\\*)".r
    val code = parseInput("input.txt")
      .map(line => {
        val customIntAdded = regex.replaceAllIn(line, "(CustomInt($1))")
        val operatorsOverloaeded = regexOperator.replaceAllIn(customIntAdded , ".$1")
        "sum = (" + operatorsOverloaeded  + ") + sum"
      })
      

    val preprendedcode = """
import scala.io.Source

case class CustomInt(i: Long) {

  def +(v2:CustomInt): CustomInt = {
    //println(this.i + " + " + v2.i)
    CustomInt(this.i+v2.i)
  }

  def +(v2: Long): Long = {
    this.i+v2
  }
  
  def *(v2:CustomInt): CustomInt = {
    //println(this.i + " * " + v2.i)
    CustomInt(this.i*v2.i)
  }
}

"""
    val code1 = for((value, i)<- code.zipWithIndex; if i< code.size/2 ) yield value
    val code2 = for((value, i)<- code.zipWithIndex; if i>= code.size/2 ) yield value

    val codeComplete1 = preprendedcode + "var sum:Long = 0L\n" + code1.reduce(_ +"\n"+_) + " \nsum"
    val codeComplete2 = preprendedcode + "var sum:Long = 0L\n" + code2.reduce(_ +"\n"+_) + " \nsum"

    /*println(codeComplete1.substring(codeComplete1.lastIndexOf("\n")-2000, codeComplete1.size))
    println("=============")
    println(codeComplete2.substring(0, 1000))
    */
    
    val toolbox = currentMirror.mkToolBox()

    val result1 = toolbox.compile(toolbox.parse(codeComplete1))()
    val toolbox2 = currentMirror.mkToolBox()
    val result2 = toolbox2.compile(toolbox.parse(codeComplete2))()
    println(result1)
    println(result2)

  }
}

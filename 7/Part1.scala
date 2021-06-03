import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {

  val objectPattern = "([0-9]+) ([a-z]+ [a-z]+)".r

  def main(args: Array[String]): Unit = {
    val lines = parseInput("input.txt")

    val t1 = System.nanoTime

    var resultMap: scala.collection.mutable.Map[String, Bag] = scala.collection.mutable.Map("shiny gold" -> Bag(ListBuffer(), "shiny gold", ListBuffer()))

    val bags = lines.foreach(line => {
      val lineParts = line.split("bags contain")
      val parent = lineParts(0).trim()
      val children = lineParts(1)
        .trim()
        .replace(".", "")
        .split(",").toSeq
        .flatMap(child => {
          if (child.contains("no other bags")) {
            None
          } else {
            val trimmedChild =
              child.replace("bags", "").replace("bag", "").trim()
            val objectPattern(count: String, bagtype: String) = trimmedChild
            Some((count.toInt, bagtype))
          }
        }).filter(str => !str._2.isEmpty())
        .foreach(childTuple => {
          val child = childTuple._2
          val childcount = childTuple._1
          resultMap.getOrElseUpdate(child, Bag(ListBuffer(), child, ListBuffer())).parents.+=(resultMap.getOrElseUpdate(parent, Bag(ListBuffer(), parent, ListBuffer())))
          resultMap.getOrElseUpdate(parent, Bag(ListBuffer(), parent, ListBuffer())).children.+=((childcount, resultMap.getOrElseUpdate(child, Bag(ListBuffer(), child, ListBuffer()))))         
        })
    })
    
    val set: mutable.Set[String] = mutable.Set()
    countParentsNested(resultMap, "shiny gold", set)
    println(set.size)
    
    println(countChildrenNested(resultMap.get("shiny gold").get))

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
  }

  def countParentsNested(map: scala.collection.mutable.Map[String, Bag], key: String, set: mutable.Set[String]): Unit = {
    val parents = map.get(key).get.parents
    for(parent <- parents) {
      set.add(parent.name)
      countParentsNested(map, parent.name, set)
    }
  }

  def countChildrenNested(bag: Bag):Int = {
    val children = bag.children
    var counter = 1
    for (child<-children) {
      counter += (child._1*countChildrenNested(child._2))
    }
    counter
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }
}

case class Bag(var parents: ListBuffer[Bag], val name: String, var children: ListBuffer[Tuple2[Int,Bag]])
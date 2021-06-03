import scala.io.Source
import java.util.concurrent.TimeUnit

object Part2 {
  def main(args: Array[String]): Unit = {
    val content = parseInput("input.txt")

    val t1 = System.nanoTime
    val groupLists = content
      .split("\n\n")
      .map(entry => entry.split("\n"))

    val counts = groupLists.map(group => {
      if (group.size == 1) {
        group(0).size
      } else {
        var map = collection.mutable.Map[Char, Int]()
        for (entry <- group) {
          entry.trim.foreach(letter =>
            map += (letter -> (1 + map.get(letter).getOrElse(0)))
          )
        }
        val size = map.filter { case (k: Char, v: Int) => v == group.size }.toSeq.size
        if(group(0) == "fbqjswm"){
          println(size)
          println(group.size)
          println(map)
        }
        size
      }
    })
  

    val sum = counts.sum

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println("sum " + sum)
  }
  /*
  def getGroupCounts(group: Iterator[String], hitmap: Map[Char, Int], count: Int) = {
    if(group.hasNext) {
      group.next()
    }
  }

  }
   */
  def parseInput(filename: String): String = {
    Source.fromFile(filename).getLines().mkString("\n")
  }
}

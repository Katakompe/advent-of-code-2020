import scala.io.Source
import java.util.concurrent.TimeUnit
import java.util.HashSet
import scala.util.control.Breaks._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
object Part1 {

  def main(args: Array[String]): Unit = {
    val input = parseInput("input.txt")

    val t1 = System.nanoTime

    val (rulesStr, tailList) = input.span(_ != "")
    val (myTicketUncleaned, otherTicketsUncleaned) = tailList.tail.span(_ != "")
    val myTicket =
      myTicketUncleaned.tail.map(ticket => ticket.split(",").map(_.toInt))

    val otherTickets = otherTicketsUncleaned.tail.tail.map(ticket => {
      ticket.split(",").map(_.toInt)
    })

    val rules = rulesStr.map(rule => {
      val parts = rule.split(": ")
      val name = parts(0)
      val ranges = parts(1)
        .split(" or ")
        .map(range => {
          val rangeList = range.split("-")
          (rangeList(0).toInt, rangeList(1).toInt)
        })
      Rule(name, ranges)
    })

    val result =
      for (ticket <- otherTickets) yield invalidTicketErrorSum(ticket, rules)
    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result.sum)
  }

  def invalidTicketErrorSum(ticket: Seq[Int], rules: Seq[Rule]) = {
    ticket.map( value => {
      val valid = rules.map(rule => rule.matches(value)).reduce(_||_) 
      if(valid) 0 else value
    }).sum
  }

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

  case class Rule(val name: String, val validRanges: Seq[Tuple2[Int, Int]]) {
    def matches(value: Int): Boolean = {
      val valid = (for (
        range <- validRanges;
        if (value >= range._1 && value <= range._2)
      ) yield true).size != 0
      valid
    }

  }

}

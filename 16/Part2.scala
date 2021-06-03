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

    val validTickets =
      for (ticket <- otherTickets; if invalidTicketErrorSum(ticket, rules) == 0)
        yield ticket

    val mappings =
      for (ticket <- otherTickets)
        yield for (
          (value, i) <- ticket.zipWithIndex;
          rule <- rules;
          if (rule.matches(value))
        ) yield (i, rule.name)

    val titlePositions: Array[String] = new Array[String](20)

    val orderingMap = mappings
      .map(mapping => {
        val mapped =
          mapping.groupBy(tuple => tuple._1).mapValues(_.map(_._2)).toSeq
        mapped.foreach(value =>
          if (value._2.size == 1) { titlePositions(value._1) = value._2(0) }
        )
        mapped
      })
      .reduce((m1, m2) => m1 ++ m2)
      .groupBy(_._1)
      .mapValues(v => v.map(_._2).reduce(_.intersect(_)))
      .toSeq

    var sortedOrderingMap = orderingMap.sortBy(_._2.size)

    var usedEntries = Set[String]()
    for (i <- 0 to 19) {
      for (entry <- sortedOrderingMap) {
        if (entry._2.size == 1) {
          titlePositions(entry._1) = entry._2(0)
          usedEntries = usedEntries.union(Set(entry._2(0)))
          sortedOrderingMap = sortedOrderingMap.map(e =>
            (e._1, e._2.filter(num => !usedEntries.contains(num)))
          )
        }
      }
    }

    var prod = 1L
    for (
      (value, i) <- titlePositions.zipWithIndex;
      if (value.startsWith("departure"))
    ) {
      prod = prod * myTicket(0)(i)
    }

    titlePositions.foreach(println(_))

    val result = 0
    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(prod)
  }

  def invalidTicketErrorSum(ticket: Seq[Int], rules: Seq[Rule]) = {
    ticket
      .map(value => {
        val valid = rules.map(rule => rule.matches(value)).reduce(_ || _)
        if (valid) 0 else value
      })
      .sum
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

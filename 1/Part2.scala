import scala.io.Source

object Part2 {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line.toInt).toSeq

    val result =
      (for (
        a <- lines;
        b <- lines;
        c <- lines
        if a + b + c == 2020
      ) yield a * b * c).distinct

    if (result.size != 1) {
      println("Error invalid number of results found: " + result.size)
    } else {
      println(result(0))
    }

  }
}

import scala.io.Source

object Part1Efficient {
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line.toInt).toIterator

    val result =
      (for {
        a <- lines
        b <- lines
        if a + b == 2020
      } yield a * b).next()

      println(result)


  }

  def sumDigit(n: Int): Int = {
    return if (n % 9 == 0 && n != 0) 9 else n % 9;
  }

}

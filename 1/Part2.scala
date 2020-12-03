import scala.io.Source
import java.util.concurrent.TimeUnit

object Part2 {
  var ct = 0
  def main(args: Array[String]): Unit = {
    val filename = "input.txt"
    val lines =
      (for (line <- Source.fromFile(filename).getLines())
        yield line.toInt).toSeq.map(noisyInt)
    val t1 = System.nanoTime
    val result =
      (for (
        a <- lines;
        b <- lines;
        c <- lines
        if a() + b() + c() == 2020
      ) yield a() * b() * c())

    if (result.size != 1) {
      println("Error invalid number of results found: " + result.size)
    } else {
      println(result(0))
    }

    println(lines.size)

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)

    println(ct)
  }
  def noisyInt(i: Int) = () => { ct+=1;if (i>2019) println(i); i }
}

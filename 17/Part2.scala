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

    //arbitrary size that is large enough
    var world: Array[Array[Array[Array[Char]]]] =
      Array.ofDim[Char](20, 20, 20, 20)

    for (
      i <- 6 to 6 + input.size - 1;
      j <- 6 to 6 + input.size - 1
    ) {
      world(i)(j)(9)(9) = input(i - 6)(j - 6)
    }
    world =
      world.map(_.map(_.map(_.map(ch => if (ch == '\u0000') '.' else ch))))
    for (turn <- 0 until 6) {
      var worldNew = world.map(_.map(_.map(_.clone)))
      for (
        i <- 1 until 19;
        j <- 1 until 19;
        k <- 1 until 19;
        l <- 1 until 19
      ) {
        val cube = world(i)(j)(k)(l)
        val neighbors = getNeighbors(i, j, k, l, world)
        val newCube = getNewCubeState(cube, neighbors)
        worldNew(i)(j)(k)(l) = newCube
      }
      world = worldNew
    }

    val result = world.map(_.map(_.map(_.count(_ == '#')).sum).sum).sum
    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    println(result)
  }

  def getNeighbors(
    i: Int,
    j: Int,
    k: Int,
    l: Int,
    world: Array[Array[Array[Array[Char]]]]
  ) = {
    for (
      x <- i - 1 to i + 1;
      y <- j - 1 to j + 1;
      z <- k - 1 to k + 1;
      w <- l - 1 to l + 1;
      if (!(x == i && y == j && z == k && w == l))
    ) yield world(x)(y)(z)(w)
  }

  def getNewCubeState(cube: Char, neighbors: Seq[Char]) = {
    val activeNeighbors = neighbors.count(ch => ch == '#')
    (cube, activeNeighbors) match {
      case ('#', 2) | ('#', 3) | ('.', 3) => '#'
      case ('#', _) | ('.', _)            => '.'
    }
  }

  def showSlice(k: Int, l: Int, world: Array[Array[Array[Array[Char]]]]) = {
    for (i <- 0 until 20) {
      for (j <- 0 until 20) {
      print(world(i)(j)(k)(l))
    }
    print("\n")
  }
}

  def parseInput(filename: String) = {
    (for (line <- Source.fromFile(filename).getLines())
      yield line).toSeq
  }

}

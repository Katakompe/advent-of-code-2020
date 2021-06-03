import scala.io.Source
import java.util.concurrent.TimeUnit

object Part1 {
  def main(args: Array[String]): Unit = {
    val content = parseInput("input.txt")

    val t1 = System.nanoTime
    val answersPerGroup = content.split("\n\n")
      .map(entry => entry.replace("\n", "").replace(" ", ""))
      .map(entry => entry.distinct.length())
    val sum = answersPerGroup.sum

    val duration = TimeUnit.NANOSECONDS.toMillis(System.nanoTime - t1)
    println("duration: " + duration)
    answersPerGroup.toList.foreach(el => println(el))
    println("sum " + sum)
  }

  def areFieldsValid(passport: Map[String, String]): Boolean = {
    passport.map { 
      case ("byr", v) => v.size == 4 && v.toInt > 1919 && v.toInt < 2003
      case ("iyr", v) => v.size == 4 && v.toInt > 2009 && v.toInt < 2021
      case ("eyr", v) => v.size == 4 && v.toInt > 2019 && v.toInt < 2031
      case ("hgt", v) => (v.matches("^[0-9]{3}cm") && v.substring(0,3).toInt > 149 && v.substring(0,3).toInt < 194  ) ||
                         (v.matches("^[0-9]{2}in") && v.substring(0,2).toInt >58 && v.substring(0,2).toInt < 77)
      case ("hcl", v) => v.matches("^#[0-9A-Fa-f]{6}$")
      case ("ecl", v) => v.matches("amb|blu|brn|gry|grn|hzl|oth")
      case ("pid", v) => v.matches("[0-9]{9}")
      case ("cid", v) => true
    }.reduce((a,b) => a && b)

  }

  def parseInput(filename: String): String = {
    Source.fromFile(filename).getLines().mkString("\n")
  }
}

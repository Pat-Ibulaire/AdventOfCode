import scala.annotation.tailrec
import scala.io.Source

object Day1 {
    val numbers = "123456789"
    val numbersTxt = Seq(("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9))

    def readDigit(rest: String, current: String, isFirst: Boolean): String = {
        val head::tail = rest.toList
        if (numbers.contains(head)) head.toString
        else {
            val newStr = if (isFirst) current + head.toString else head.toString + current
            numbersTxt.find(s => newStr.contains(s._1)).map(_._2.toString).getOrElse(readDigit(tail.mkString, newStr, isFirst))
        }
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day1.txt")
        val res = source.getLines().map(line => {
            val first = readDigit(line, "", true)
            val last = readDigit(line.reverse, "", false)
            (first + last).toInt
        }).sum
        source.close()
        println(res)
    }
}
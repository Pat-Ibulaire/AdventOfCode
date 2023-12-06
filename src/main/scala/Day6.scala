import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day6 {


    def resolve(currentSpeed: Long, maxTime: Long, record: Long, lastOne: Long, previous: Long, currentSuccess: Long): Long = {
        val res = (maxTime - currentSpeed) * currentSpeed
        if (res == lastOne) currentSuccess * 2
        else if (res == previous) currentSuccess * 2 - 1
        else resolve(currentSpeed+1, maxTime, record, res, lastOne, currentSuccess + (if (res > record) 1 else 0))
    }
    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day6full.txt")
        val lines = source.getLines().toList
        val races = lines.map(line => List(line.split(' ').toList.filter(_.nonEmpty).drop(1).mkString.toLong)).transpose

        val nbWaysToWin2 = races.map{ case List(time, record) => resolve(0, time, record, -1, -2, 0)}

        source.close()
        println(nbWaysToWin2.head)
    }
}
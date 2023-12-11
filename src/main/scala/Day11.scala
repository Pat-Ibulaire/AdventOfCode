import scala.collection.immutable.{HashMap, List}
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day11 {

    def expand(universe: List[String]): List[String] = universe.foldLeft(List[String]())((acc, v) => if (v.forall(c => "8.".contains(c))) acc :+ v.map(_ => '8').mkString else acc :+ v)

    def getNb8(firstLine: String, v1: Int, v2: Int): Long = firstLine.substring(Seq(v1, v2).min, Seq(v1, v2).max).count(_ == '8').toLong
    def getCouples(rest: List[(Int, Int)], built: List[((Int, Int), (Int, Int))]): List[((Int, Int), (Int, Int))] = rest match {
        case Nil => built
        case head::tail => getCouples(tail, tail.map(t => (head, t)) ++ built)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day11full.txt")
        val lines = source.getLines().toList
        val expanded = expand(expand(lines).transpose.map(_.mkString)).transpose.map(_.mkString)

        val galaxies = expanded.zipWithIndex.flatMap{ case (line, y) => line.zipWithIndex.flatMap{ case (car, x) => if (".8".contains(car)) List() else List((x, y))}}
//        println(galaxies)
        val allCouples = getCouples(galaxies, List())

        val firstLine = expanded.head
        val firstCol = expanded.transpose.head.mkString
        println(s"firstLine $firstLine")
        println(s"firstCol $firstCol")

        val res = allCouples.map{ case ((x1, y1), (x2, y2)) => {
//            println(((x1, y1), (x2, y2)))
            val nb8X = getNb8(firstLine, x1, x2)
            val nb8Y = getNb8(firstCol, y1, y2)
            (Math.abs(x2-x1) + Math.abs(y2-y1)).toLong - nb8X - nb8Y + 1000000.toLong*nb8X + 1000000.toLong*nb8Y
        }}.sum
        println(allCouples.length)
        source.close()
        println(res)
    }
}
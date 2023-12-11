import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day9 {

    def processLine(line: Seq[Long], firsts: Seq[Long]): Long = if (line.forall(_ == 0.toLong)) {
        firsts.init.foldRight(firsts.last)((thisHead, previousHead) => thisHead - previousHead)
    } else {
        val diffs = line.drop(1).foldLeft((Seq[Long](), line.head)){ case ((list, last), newVal) => (list :+ (newVal - last), newVal)}._1
        processLine(diffs, firsts :+ diffs.head)
    }
    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day9full.txt")
        val lines = source.getLines().toList
        val res = lines.map(line => {
            val splitted = line.split(' ').toList.map(_.toLong)
            processLine(splitted, Seq(splitted.head))
        })
//        println(res)


        source.close()
        println(res.sum)
    }
}
import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day8 {

    def letswalk(current: Seq[String], lr: String, currentLR: Int, done: Long, carte: Map[String, String]): Long = if (current.forall(_.last == 'Z')) done else {
        val nextPosAll = current.map(cur => carte(lr(currentLR) + cur))
        if (nextPosAll.count(_.last == 'Z') == 5) {
            println(nextPosAll)
        }
        letswalk(nextPosAll, lr, (currentLR+1) % lr.length, done+1, carte)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day8full.txt")
        val lines = source.getLines().toList
        val lr = lines.head
        val carte = lines.drop(2).flatMap(l => {
            val List(current, leftRight) = l.split(" = ", 1000).toList
            val List(left, right) = leftRight.replace("(", "").replace(")", "").split(", ").toList
            List(("L" + current, left), ("R" + current, right))
        }).toMap
        val first = carte.keys.toList.map(_.drop(1)).filter(_.last == 'A').distinct
        println(first)
        println(carte.keys.toList.filter(_.last == 'Z'))
        val res = letswalk(first, lr, 0, 0, carte)
//        println(lr)
//        println(carte)


        source.close()
        println(res)
    }
}
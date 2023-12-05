import scala.collection.mutable
import scala.io.Source

object Day4 {

    def getScore(winners: Seq[String], rest: List[String], score: Int): Int = rest match {
        case Nil => score
        case head::tail =>
            val newScore = if (winners.contains(head)) score+1 else score
            getScore(if (newScore != score) winners.filter(w => w != head) else winners, tail, newScore)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day4full.txt")
        val lines = source.getLines()
        val cardInstances = new mutable.HashMap[Int, Int]
        val res = lines.zipWithIndex.map{ case (l, cardIdx) => {
            val Seq(winners, challengers) = l.split(':').last.split('|').toSeq
            val score = getScore(winners.trim.split(' ').filter(_ != ""), challengers.trim.split(' ').filter(_ != "").toList, 0)
            val thisCardInstances = cardInstances.getOrElse(cardIdx, 1)
            if (score > 0) {
                (1 to score).foreach(x => cardInstances.put(cardIdx+x, cardInstances.getOrElse(cardIdx+x, 1)+thisCardInstances))
            }
            println(s"Card nb ${cardIdx+1} => $thisCardInstances")
            thisCardInstances
        }}.sum
        source.close()
        println(res)
    }
}
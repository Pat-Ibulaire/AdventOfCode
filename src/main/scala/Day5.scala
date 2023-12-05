import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day5 {

    def groupSuccBy[A](list: List[A], acc: List[List[A]], isSameGroup: (A, A) => Boolean): List[List[A]] = {
        list match {
            case Nil => acc.map(_.reverse).reverse
            case elem :: reste => acc match {
                case Nil => groupSuccBy(reste, List(List(elem)), isSameGroup)
                case (t :: q) :: others => {
                    if (isSameGroup(elem, t)) groupSuccBy(reste, (elem :: t :: q) :: others, isSameGroup) else groupSuccBy(reste, List(elem) :: (t :: q) :: others, isSameGroup)
                }
            }
        }
    }

    def nextClassGroup(seedStart: Long, seedEnd: Long, dic: List[List[Long]]): List[(Long, Long)] = {
        val startFoundRule = dic.find{ case List(destStart, sourceStart, len) => seedStart >= sourceStart && seedStart <= sourceStart + len-1 }
        val endFoundRule = dic.find{ case List(destStart, sourceStart, len) => seedEnd >= sourceStart && seedEnd <= sourceStart + len-1 }
        (startFoundRule, endFoundRule) match {
            case (None, None) => List((seedStart, seedEnd))
            case (Some(rule), None) =>
                val List(destStart, sourceStart, len) = rule
                List(changeWithRule((seedStart, sourceStart + len-1), rule), (sourceStart + len-1, seedEnd))
            case (None, Some(rule)) =>
                val List(destStart, sourceStart, len) = rule
                List((seedStart, sourceStart), changeWithRule((sourceStart, seedEnd), rule))
            case (Some(rule1), Some(rule2)) if rule1.head == rule2.head && rule1(1) == rule2(1) && rule1(2) == rule2(2) =>
                List(changeWithRule((seedStart, seedEnd), rule1))
            case _ =>
                val List(destStart1, sourceStart1, len1) = startFoundRule.get
                val List(destStart2, sourceStart2, len2) = endFoundRule.get
                List(changeWithRule((seedStart, sourceStart1+len1-1), startFoundRule.get)) ++
                    (if (sourceStart1+len1-1+1 == sourceStart2) List() else nextClassGroup(sourceStart1+len1-1+1, sourceStart2-1, dic)) ++
                    List(changeWithRule((sourceStart2, seedEnd), endFoundRule.get))
        }
    }

    def changeWithRule(seedGroup: (Long, Long), rule: List[Long]): (Long, Long) = rule match {
        case List(destStart, sourceStart, _) => (destStart + seedGroup._1 - sourceStart, destStart + seedGroup._2 - sourceStart)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day5full.txt")
        val lines = source.getLines().toList
        val seedsLine = lines.head.split(':').last.trim.split(' ').map(_.toLong)
        val seedsGroups = seedsLine.toList.grouped(2).toList.map{ case List(first, nb) => (first, first+nb) }

        val parts = groupSuccBy[String](lines.drop(2), List(), (_, l2) => l2 != "").map(_.filter(line => Try(line.head.toString.toLong).toOption.isDefined).map(line => {
            line.split(' ').map(_.toLong).toList
        }))

        val res = parts.foldLeft(seedsGroups){ case (currentGroups, dic) => currentGroups.flatMap(couple => nextClassGroup(couple._1, couple._2, dic))}.minBy(_._1)._1

        source.close()
        println(res)
    }
}
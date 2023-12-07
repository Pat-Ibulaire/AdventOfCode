import scala.collection.immutable.List
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day7 {

    val cards = "J23456789TQKA"

    def strength(card: Char): Int = cards.indexOf(card) + 1

    val types = List("RIEN", "PAIRE", "DEUX", "BRELAN", "FULL", "CARRE", "FIVE")
    def typeStrength(t: String): Int = types.indexOf(t) + 1

    def sortHand(hand: String): String = hand.sortBy(c => {
        val count = hand.count(c2 => c == c2)
        if (count > 2) 10000 * strength(c) else if (count == 2) 100 * strength(c) else strength(c)
    }).reverse

    def handTypeStrength(hand: String): String = {
        val convertedHand = if (hand.contains("J") && hand != "JJJJJ") {
            val maxChar = hand.filter(_ != 'J').toList.distinct.maxBy(c => hand.count(c2 => c == c2))
            val r = hand.replace('J', maxChar)
            println(s"$hand ==> $r")
            r
        } else hand
        sortHand(convertedHand).toList match {
            case a::b::c::d::e::_ if a == b && a == c && a == d && a == e => "FIVE"
            case a::b::c::d::_ if a == b && a == c && a == d => "CARRE"
            case a::b::c::d::e::_ if a == b && a == c && d == e => "FULL"
            case a::b::c::_ if a == b && a == c => "BRELAN"
            case a::b::c::d::_ if a == b && c == d => "DEUX"
            case a::b::_ if a == b => "PAIRE"
            case _ => "RIEN"
        }
    }

    def restStrength(hand: String): Long = {
        val a::b::c::d::e::_ = hand.toList
        strength (a) * 1000000 + strength (b) * 50000 + strength (c) * 1000 + strength (d) * 20 + strength (e)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day7full.txt")
        val lines = source.getLines().toList
        val res = lines.map(_.split(' ').toList).map{ case List(hand, bid) => (hand, bid, handTypeStrength(hand), restStrength(hand))}
            .groupBy(_._3).values.toList.map(listOfSameClass =>
                listOfSameClass.sortBy(_._4)
            ).sortBy(x => typeStrength(x.head._3))
            .flatten.zipWithIndex.map{ case ((hand, bid, _, _), idx) =>
            println(s"${idx+1} ${sortHand(hand)} ($hand), type : ${handTypeStrength(hand)}, rest : ${restStrength(hand)}")
            bid.toLong * (idx + 1).toLong
        }.sum
        source.close()
        println(res)
    }
}
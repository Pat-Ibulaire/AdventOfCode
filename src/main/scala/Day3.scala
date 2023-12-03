import scala.io.Source
import scala.util.Try

object Day3 {

    def groupSuccBy[A](list: Seq[A], acc: Seq[Seq[A]], isSameGroup: (A, A) => Boolean): Seq[Seq[A]] = {
        list match {
            case Nil => acc.map(_.reverse).reverse
            case elem :: reste => acc match {
                case Nil => groupSuccBy(reste, Seq(Seq(elem)), isSameGroup)
                case (t :: q) :: others => {
                    if (isSameGroup(elem, t)) groupSuccBy(reste, (elem :: t :: q) :: others, isSameGroup) else groupSuccBy(reste, Seq(elem) :: (t :: q) :: others, isSameGroup)
                }
            }
        }
    }

    def findStart(picture: Seq[Seq[(Int, Int, Char)]], number: (Int, Int, String)): Option[(Int, Int, Char)] = {
        val (x, y, num) = number
        (y-1 to y+num.length).find(y2 => Try(picture(x-1)(y2)._3).toOption.getOrElse('.') == '*').map(y2 => (x-1, y2, '*')).orElse(
            Seq(y - 1, y + num.length).find(y2 => Try(picture(x)(y2)._3).toOption.getOrElse('.') == '*').map(y2 => (x, y2, '*')).orElse(
                (y - 1 to y + num.length).find(y2 => Try(picture(x + 1)(y2)._3).toOption.getOrElse('.') == '*').map(y2 => (x+1, y2, '*'))
            )
        )
    }

    def processStars(allStars: List[((Int, Int, String), (Int, Int, Char))], res: Int): Int = allStars match {
        case Nil => res
        case head::rest =>
            val (found, other) = rest.partition{ case ((_, _, _), (xStar, yStar, _)) => head._2._1 == xStar && head._2._2 == yStar }
            if (found.nonEmpty) {
                val gearRatio = head._1._3.toInt * found.head._1._3.toInt
                processStars(other, res + gearRatio)
            } else processStars(rest, res)
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day3full.txt")
        val lines = source.getLines().toSeq
        val picture = lines.zipWithIndex.map{ case (l, x) => l.zipWithIndex.map{ case (char, y) => (x, y, char) }}
        val grouped = groupSuccBy[(Int, Int, Char)](picture.flatten.filter(t => Try(t._3.toString.toInt).toOption.isDefined), Seq(), { case ((x, y, _), (x2, y2, _)) => x==x2 && (y==y2-1 || y==y2+1) })
        val numbersWithIndex = grouped.map(numbers => (numbers.head._1, numbers.head._2, numbers.map(_._3).mkString))


        val res = numbersWithIndex.map(num => (num, findStart(picture, num))).filter(_._2.isDefined).map(v => (v._1, v._2.get))
        source.close()
        println(processStars(res.toList, 0))
    }
}
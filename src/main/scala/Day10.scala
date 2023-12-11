import scala.collection.immutable.{HashMap, List}
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object Day10 {

    def followPipe(x: Int, y: Int, previousX: Int, previousY: Int, soFar: Seq[((Int, Int), Char)], map: mutable.HashMap[String, Char]): Map[(Int, Int), Char] = {
        val pipe = map(s"$x,$y")
        if (pipe == 'S') soFar.toMap else {
            val (x1, y1, x2, y2) = pipe match {
                case 'L' => (x, y-1, x+1, y)
                case 'F' => (x+1, y, x, y+1)
                case 'J' => (x, y-1, x-1, y)
                case '7' => (x-1, y, x, y+1)
                case '|' => (x, y-1, x, y+1)
                case '-' => (x-1, y, x+1, y)
            }
            val (nextX, nextY) = if (previousX == x1 && previousY == y1) (x2, y2) else (x1, y1)
            followPipe(nextX, nextY, x, y, ((x, y), pipe) +: soFar, map)
        }
    }

    def countNest(line: List[(Int, Int, Char)], isIn: Boolean, previous: Char, previousTurn: Char, soFar: Int, mapIn: Map[(Int, Int), Char]): Int = line match {
        case Nil => soFar
        case head::tail => (head._3, isIn, mapIn.contains((head._1, head._2))) match {
            case ('-', _, true) => countNest(tail, isIn, '-', previousTurn, soFar, mapIn)
            case ('|', _, true) => countNest(tail, !isIn, '|', previousTurn, soFar, mapIn)
            case ('7', _, true) => countNest(tail, if (previousTurn == 'L') isIn else !isIn, previous, '7', soFar, mapIn)
            case ('J', _, true) => countNest(tail, if (previousTurn == 'F') isIn else !isIn, previous, 'J', soFar, mapIn)
            case (_, _, true) if Seq('L', 'F').contains(head._3) => countNest(tail, !isIn, head._3, head._3, soFar, mapIn)
            case _ => countNest(tail, isIn, head._3, previousTurn, soFar+(if (isIn) 1 else 0), mapIn)
        }
    }

    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day10full.txt")
        val lines = source.getLines().toList

        var startX = 0
        var startY = 0
        val map = new mutable.HashMap[String, Char]()
        lines.zipWithIndex.foreach { case (line, y) => line.zipWithIndex.foreach { case (car, x) =>
            if (car == 'S') {
                startX = x
                startY = y
            }
            map.put(s"$x,$y", car)
        }}
        val (nextX, nextY) = Seq(
            map.get(s"${startX-1},${startY}").filter(c => Seq('F', 'L', '-').contains(c)).map(_ => (startX-1, startY)), // gauche
            map.get(s"${startX+1},${startY}").filter(c => Seq('J', '7', '-').contains(c)).map(_ => (startX+1, startY)), // droite
            map.get(s"${startX},${startY-1}").filter(c => Seq('F', '7', '|').contains(c)).map(_ => (startX, startY-1)), // haut
            map.get(s"${startX},${startY+1}").filter(c => Seq('L', 'J', '|').contains(c)).map(_ => (startX, startY+1)) // bas
        ).flatten.head

        val allPipes = followPipe(nextX, nextY, startX, startY, Seq(((startX, startY), 'S')), map)
        val res = lines.zipWithIndex.map{ case (line, y) => {
            val count = countNest(line.replace("S", "F").zipWithIndex.map{ case (car, x) => (x, y, car) }.toList, false, '.', '.', 0, allPipes)
//            println(s"line $line => " + count)
            count
        }}.sum



        source.close()
        println(res)
    }
}
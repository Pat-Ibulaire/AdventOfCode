import scala.io.Source

object Day2 {

    def getPower(cubes: String): Int = {
        val (maxRed, maxGreen, maxBlue) = cubes.split(';').flatMap(_.split(',')).foldLeft((0, 0, 0)){ case ((red, green, blue), cube) => {
            cube.trim.split(' ').toList match {
                case nb::"red"::_ => (Seq(nb.toInt, red).max, green, blue)
                case nb::"green"::_ => (red, Seq(nb.toInt, green).max, blue)
                case nb::"blue"::_ => (red, green, Seq(nb.toInt, blue).max)
            }
        }}
        maxRed * maxGreen * maxBlue
    }


    def main(args: Array[String]) = {
        val source = Source.fromFile("/home/tpasquet/tmp/day2.txt")
        val lines = source.getLines()
        val res = lines.map(l => (l.split(':').head.split(' ')(1).toInt, l.split(':')(1))).map{ case (_, cubes) => getPower(cubes)}.sum
        source.close()
        println(res)
    }
}
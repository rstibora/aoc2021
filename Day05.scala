import scala.io.Source
import scala.collection.Map


case class Point(x: Int, y: Int)
case class Line(pointA: Point, pointB: Point)
type CloudDensity = Map[Point, Int]

object Line:
    def from(input: String): Line =
        input match
            case s"$xa,$ya -> $xb,$yb" => Line(Point(xa.toInt, ya.toInt), Point(xb.toInt, yb.toInt))
            case _ => throw Exception(s"Can't parse Line from $input.")

extension (line: Line)
    def toPoints: Seq[Point] =
        if line.pointA.x == line.pointB.x then
            (if line.pointA.y <= line.pointB.y then line.pointA.y to line.pointB.y else line.pointA.y to line.pointB.y by -1)
                .map(yCoord => Point(line.pointA.x, yCoord))
        else if line.pointA.y == line.pointB.y then
            (if line.pointA.x <= line.pointB.x then line.pointA.x to line.pointB.x else line.pointA.x to line.pointB.x by -1)
                .map(xCoord => Point(xCoord, line.pointA.y))
        else
            Seq()

@main def firstStar() =
    val lines = Source.fromFile("./inputs/day05").getLines.map(Line.from).toSeq
    var clouds: CloudDensity = Map()

    def processLine(lines: Seq[Line], clouds: CloudDensity): CloudDensity =
        if lines.isEmpty then return clouds
        processLine(lines.tail, clouds ++ lines.head.toPoints.map(point => (point, clouds.getOrElse(point, 0) + 1)))
    clouds = processLine(lines, clouds)
    println(clouds.values.filter(density => density >= 2).size)

package aoc2021.day5

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
    def toPoints(skipDiagonals: Boolean): Seq[Point] =
        if skipDiagonals && (line.pointA.x != line.pointB.x && line.pointA.y != line.pointB.y) then
            return Seq()

        val deltaX = (line.pointB.x - line.pointA.x).sign
        val deltaY = (line.pointB.y - line.pointA.y).sign
        val lengthX = line.pointA.x.max(line.pointB.x) - line.pointA.x.min(line.pointB.x)
        val lengthY = line.pointA.y.max(line.pointB.y) - line.pointA.y.min(line.pointB.y)
        val length = lengthX.max(lengthY)

        (0 to length).map(step => Point(line.pointA.x + step * deltaX, line.pointA.y + step * deltaY))

def firstStar(inputLines: Seq[String]): Long =
    val lines = inputLines.map(Line.from).toSeq
    var clouds: CloudDensity = Map()

    def processLine(lines: Seq[Line], clouds: CloudDensity): CloudDensity =
        if lines.isEmpty then return clouds
        processLine(lines.tail, clouds ++ lines.head.toPoints(skipDiagonals = true).map(point => (point, clouds.getOrElse(point, 0) + 1)))
    clouds = processLine(lines, clouds)
    clouds.values.filter(density => density >= 2).size

def secondStar(inputLines: Seq[String]): Long =
    val lines = inputLines.map(Line.from).toSeq
    var clouds: CloudDensity = Map()

    def processLine(lines: Seq[Line], clouds: CloudDensity): CloudDensity =
        if lines.isEmpty then return clouds
        processLine(lines.tail, clouds ++ lines.head.toPoints(skipDiagonals = false).map(point => (point, clouds.getOrElse(point, 0) + 1)))
    clouds = processLine(lines, clouds)
    clouds.values.filter(density => density >= 2).size

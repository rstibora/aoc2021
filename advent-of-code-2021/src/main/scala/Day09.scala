package aoc2021.day9

case class Point(x: Int, y: Int, height: Int)

object Point:
    def neighbours(point: Point, points: Set[Point]): Set[Point] =
        points.filter(testedPoint => (testedPoint.x == point.x && (testedPoint.y - point.y).abs == 1)
                                  || (testedPoint.y == point.y && (testedPoint.x - point.x).abs == 1))

def firstStar(inputLines: Seq[String]): Long =
    val points = inputLines.zipWithIndex.map(
        (line, y) => line.zipWithIndex.map(
            (height, x) => Point(x, y, height.asDigit)).toSet).reduce(_ ++ _)

    def lowPoints(height: Int, points: Set[Point], pointsAboveHeight: Set[Point], lowPointsAboveHeight: Set[Point]): Set[Point] =
        if height < -1
            then return lowPointsAboveHeight

        val pointsAtHeight = points.filter(_.height == height)
        val lowPointsAtHeight = pointsAtHeight.filter(point => Point.neighbours(point, points) == (Point.neighbours(point, points) & pointsAboveHeight))
        lowPoints(height - 1, points, pointsAboveHeight ++ pointsAtHeight, lowPointsAboveHeight ++ lowPointsAtHeight)

    lowPoints(points.maxBy(_.height).height, points, Set[Point](), Set[Point]()).toSeq.map(_.height + 1).reduce(_ + _)

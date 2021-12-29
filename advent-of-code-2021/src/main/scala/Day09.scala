package aoc2021.day9

case class Point(x: Long, y: Long)

type PointsWithHeight = Map[Point, Long]

object Point:
    def neighbours(point: Point, points: Set[Point]): Set[Point] =
        points.filter(testedPoint => (testedPoint.x == point.x && (testedPoint.y - point.y).abs == 1)
                                  || (testedPoint.y == point.y && (testedPoint.x - point.x).abs == 1))

def parseInput(inputLines: Seq[String]): PointsWithHeight =
    inputLines.zipWithIndex.map(
        (line, y) => line.zipWithIndex.map(
            (height, x) => Point(x, y) -> height.asDigit.toLong).toMap).reduce(_ ++ _)

def lowPointsUpToHeight(height: Long, points: PointsWithHeight, pointsAboveHeight: Set[Point], lowPointsAboveHeight: PointsWithHeight): PointsWithHeight =
    if height < -1
        then return lowPointsAboveHeight

    val pointsAtHeight = points.filter((_, testedHeight) => testedHeight == height)
    val lowPointsAtHeight = pointsAtHeight.filter(
        (point, _) => Point.neighbours(point, points.keySet) == (Point.neighbours(point, points.keySet) & pointsAboveHeight))
    lowPointsUpToHeight(height - 1, points, pointsAboveHeight ++ pointsAtHeight.keySet, lowPointsAboveHeight ++ lowPointsAtHeight)

def firstStar(inputLines: Seq[String]): Long =
    val points = parseInput(inputLines)
    lowPointsUpToHeight(9, points, Set[Point](), Map[Point, Long]()).values.map(_ + 1).reduce(_ + _)

def secondStar(inputLines: Seq[String]): Long =
    val points = parseInput(inputLines)
    val lowPoints = lowPointsUpToHeight(9, points, Set[Point](), Map[Point, Long]())

    def basinForPoint(point: Point, points: PointsWithHeight): Set[Point] =
        def connectedComponent(pointsToProcess: Set[Point], points: Set[Point], component: Set[Point]): Set[Point] =
            if pointsToProcess.size == 0 then component
            else
                val pickedPointNeighbours = Point.neighbours(pointsToProcess.head, points)
                connectedComponent(pointsToProcess.tail ++ pickedPointNeighbours,
                                   points -- pickedPointNeighbours,
                                   component + pointsToProcess.head)
        connectedComponent(Set(point), points.filter((_, height) => height < 9).keySet, Set())

    val basins = lowPoints.map((point, _) => basinForPoint(point, points))
    basins.map(_.size).toVector.sorted.reverse.take(3).reduce(_ * _)

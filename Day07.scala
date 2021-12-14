import scala.io.Source


@main def firstStar() =
    val positions = Source.fromFile("./inputs/day07").getLines.next.split(",").map(_.toInt)
    val minPosition = positions.min
    val maxPosition = positions.max
    val positionPrices = (minPosition to maxPosition).map(
        possiblePosition => (possiblePosition,
                             positions.map(position => (position - possiblePosition).abs).sum))
    println(positionPrices.minBy((position, price) => price)._2)

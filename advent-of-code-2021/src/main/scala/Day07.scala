package aoc2021.day7

import scala.io.Source


def firstStar(inputLines: Seq[String]): Long =
    val positions = inputLines.head.split(",").map(_.toInt)
    val minPosition = positions.min
    val maxPosition = positions.max

    val positionPrices = (minPosition to maxPosition).map(
        possiblePosition => (possiblePosition,
                             positions.map(position => (position - possiblePosition).abs).sum))

    positionPrices.minBy((position, price) => price)._2

def secondStar(inputLines: Seq[String]): Long =
    val positions = inputLines.head.split(",").map(_.toInt)
    val minPosition = positions.min
    val maxPosition = positions.max

    val positionPrices = (minPosition to maxPosition).map(
        possiblePosition => (possiblePosition,
                             positions.map(position => 
                                 val n = (position - possiblePosition).abs
                                 (n * (n + 1)) / 2
                             ).sum))
    positionPrices.minBy((position, price) => price)._2

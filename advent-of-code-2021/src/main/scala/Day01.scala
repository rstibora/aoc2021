package aoc2021.day1

import scala.io.Source

def countDepthIncreases(depthMeasurements: Array[Int]): Int =
    var depthIncreasesCount = 0
    var previousDepth = depthMeasurements(0)
    for
        depthMeasurement <- depthMeasurements
    do
        if depthMeasurement.toInt > previousDepth then
            depthIncreasesCount += 1
        previousDepth = depthMeasurement
    return depthIncreasesCount


def filter(rawDepthMeasurements: Array[Int], windowSize: Int): Array[Int] =
    return rawDepthMeasurements.sliding(windowSize).map(window => window.sum).toArray

def firstStar(inputLines: Seq[String]): Long =
    val depthMeasurements = inputLines.map(line => line.toInt).toArray
    countDepthIncreases(depthMeasurements)

def secondStar(inputLines: Seq[String]): Long =
    val depthMeasurements = inputLines.map(line => line.toInt).toArray
    val filteredMeasurements = filter(depthMeasurements, windowSize = 3)
    countDepthIncreases(filteredMeasurements)

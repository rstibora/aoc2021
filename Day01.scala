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

def firstStar() = 
    val depthMeasurements = Source.fromFile("./inputs/day01").getLines().map(line => line.toInt).toArray
    println(countDepthIncreases(depthMeasurements))

@main def secondStar() =
    val depthMeasurements = Source.fromFile("./inputs/day01").getLines().map(line => line.toInt).toArray
    val filteredMeasurements = filter(depthMeasurements, windowSize = 3)
    println(countDepthIncreases(filteredMeasurements))

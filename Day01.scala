import scala.io.Source

@main def solution() = 
    val depthMeasurements = Source.fromFile("./inputs/day01").getLines().toArray

    var depthIncreasesCount = 0
    var previousDepth = depthMeasurements(0).toInt
    for
        depthMeasurement <- depthMeasurements
    do
        if depthMeasurement.toInt > previousDepth then 
            depthIncreasesCount += 1
        previousDepth = depthMeasurement.toInt

    println(depthIncreasesCount)

import scala.collection.Map
import scala.io.Source


def firstStar() =
    var fishTimers = Source.fromFile("./inputs/day06").getLines.next.split(",").map(_.toInt).toSeq
    for iteration <- 0 until 80 do
        fishTimers = fishTimers.filter(timer => timer == 0).map(timer => 8) ++ fishTimers.map(timer => if timer == 0 then 6 else timer - 1)
    println(fishTimers.length)

@main def secondStar() =
    val fishTimers = Source.fromFile("./inputs/day06").getLines.next.split(",").map(_.toInt)
    val fishPerTimer = Map.from((0 to 8).map(timer => (timer, 0L)))
                       ++ Map.from(fishTimers.groupBy(identity).mapValues(_.length.toLong))

    def tickTimer(fishPerTimer: Map[Int, Long]): Map[Int, Long] =
        fishPerTimer.map((timer, fishAmount) => timer match
                                                        case 8 => (8, fishPerTimer.getOrElse(0, 0L))
                                                        case 6 => (6, fishPerTimer.getOrElse(0, 0L) + fishPerTimer.getOrElse(7, 0L))
                                                        case timerValue => (timerValue, fishPerTimer.getOrElse(timerValue + 1, 0L)))

    def recursiveTickTimer(fishPerTimer: Map[Int, Long], numberOfIterations: Int): Map[Int, Long] =
        if numberOfIterations == 0 then fishPerTimer
        else recursiveTickTimer(tickTimer(fishPerTimer), numberOfIterations - 1)

    println(recursiveTickTimer(fishPerTimer, 256).values.sum)

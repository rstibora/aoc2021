import scala.io.Source


@main def firstStar() =
    var fishTimers = Source.fromFile("./inputs/day06").getLines.next.split(",").map(_.toInt).toSeq
    for
        iteration <- 0 until 80
    do
        fishTimers = fishTimers.filter(timer => timer == 0).map(timer => 8) ++ fishTimers.map(timer => if timer == 0 then 6 else timer - 1)
    println(fishTimers.length)

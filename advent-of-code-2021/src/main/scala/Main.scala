package aoc2021

import scala.io.Source

@main def main(day: Int, star: Int, inputFilePath: String): Int =

    if star != 1 && star != 2 then
        println("Star has to be either 1 or 2.")
        return 1

    val inputLines = Source.fromFile(inputFilePath).getLines.toSeq

    val solution = day match
        case 1 =>
            import aoc2021.day1.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 2 =>
            import aoc2021.day2.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 3 =>
            import aoc2021.day3.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 4 =>
            import aoc2021.day4.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 5 =>
            import aoc2021.day5.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 6 =>
            import aoc2021.day6.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 7 =>
            import aoc2021.day7.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 8 =>
            import aoc2021.day8.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case 9 =>
            import aoc2021.day9.{firstStar, secondStar}
            star match
                case 1 => firstStar(inputLines)
                case 2 => secondStar(inputLines)
        case _ =>
            println(s"Star $star solution for day $day is not implemented.")
            return 1

    println(s"Solution for day $day star $star: $solution.")
    return 0

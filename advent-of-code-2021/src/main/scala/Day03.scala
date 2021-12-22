package aoc2021.day3

import scala.io.Source
import scala.math.pow

def parseDiagnosticLine(diagnosticLine: String): Vector[Int] =
    diagnosticLine.map(digit => if digit == '1' then 1 else 0).toVector

def toDecimal(diagnosticLineBits: Vector[Int]): Int =
    diagnosticLineBits.reverse.zipWithIndex.map((bit, index) => bit * pow(2, index).intValue).sum

def findMostCommonBits(parsedDiagnosticLines: Vector[Vector[Int]]): Vector[Option[Int]] =
    val summedBits = parsedDiagnosticLines.reduce((diagnosticLineA, diagnosticLineB) => diagnosticLineA.zip(diagnosticLineB).map((a: Int, b: Int) => a + b))
    summedBits.map(diagnostiBitsCount => if diagnostiBitsCount > parsedDiagnosticLines.length - diagnostiBitsCount then Some(1)
                                         else if diagnostiBitsCount == parsedDiagnosticLines.length - diagnostiBitsCount then Option.empty
                                         else Some(0))

def findLeastCommonBits(parsedDiagnosticLines: Vector[Vector[Int]]): Vector[Option[Int]] =
    findMostCommonBits(parsedDiagnosticLines).map(_.map((bit) => if bit == 1 then 0 else 1))

def filterAccordingNthBit(diagnosticLines: Vector[Vector[Int]], bitPosition: Int, criterium: (Vector[Vector[Int]]) => Vector[Option[Int]], preferredBitValue: Int): Vector[Vector[Int]] =
    if diagnosticLines.length == 1 then return diagnosticLines

    val processedLines = criterium(diagnosticLines).map(_.getOrElse(preferredBitValue))
    diagnosticLines.filter(diagnosticLine => diagnosticLine(bitPosition) == processedLines(bitPosition))

def firstStar(inputLines: Seq[String]): Long =
    val diagnosticReport = inputLines.map(parseDiagnosticLine).toVector
    val mostCommonBits = findMostCommonBits(diagnosticReport).map(_.getOrElse(1))
    val leastCommonBits = findLeastCommonBits(diagnosticReport).map(_.getOrElse(1))
    toDecimal(mostCommonBits) * toDecimal(leastCommonBits)

def secondStar(inputLines: Seq[String]): Long =
    val diagnosticReport = inputLines.map(parseDiagnosticLine).toVector
    var filteredReport = diagnosticReport
    for bitPosition <- 0 until filteredReport(0).length do
        filteredReport = filterAccordingNthBit(filteredReport, bitPosition, findMostCommonBits, preferredBitValue = 1)
    val oxygenGeneratorRating = toDecimal(filteredReport(0))

    filteredReport = diagnosticReport
    for bitPosition <- 0 until filteredReport(0).length do
        filteredReport = filterAccordingNthBit(filteredReport, bitPosition, findLeastCommonBits, preferredBitValue = 0)
    val co2ScrubberRating =  toDecimal(filteredReport(0))
    oxygenGeneratorRating * co2ScrubberRating

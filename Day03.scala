import scala.io.Source
import scala.math.pow

def parseDiagnosticLine(diagnosticLine: String): Vector[Int] =
    return diagnosticLine.map(digit => if digit == '1' then 1 else 0).toVector

def toDecimal(diagnosticLineBits: Vector[Int]): Int =
    diagnosticLineBits.reverse.zipWithIndex.map((bit, index) => bit * pow(2, index).intValue).sum

@main def firstStar() =
    val diagnosticReport = Source.fromFile("./inputs/day03").getLines.map(parseDiagnosticLine).toVector
    val diagnosticReportLenght = diagnosticReport.length

    val summedBits = diagnosticReport.reduce((diagnosticLineA, diagnosticLineB) => diagnosticLineA.zip(diagnosticLineB).map((a: Int, b: Int) => a + b))
    val mostCommonBits = summedBits.map(diagnostiBitsCount => if diagnostiBitsCount > diagnosticReportLenght / 2 then 1 else 0)
    val leastCommonBits = mostCommonBits.map(bit => if bit == 1 then 0 else 1).toVector
    println(toDecimal(mostCommonBits) * toDecimal(leastCommonBits))

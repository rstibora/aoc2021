package aoc2021.day8

import scala.collection.Set


case class Entry(patterns: Seq[String], outputs: Seq[String])

object Entry:
    def from(entry: String): Entry =
        val splitEntry = entry.split("\\|")
        Entry(splitEntry(0).split(" ").filter(_ != "").toIndexedSeq,
              splitEntry(1).split(" ").filter(_ != "").toIndexedSeq)

def firstStar(inputLines: Seq[String]): Long =
    val entries = inputLines.map(Entry.from)
    def patternFilter(pattern: String): Boolean =
        pattern.length match
            case 2 | 3 | 4 | 7 => true
            case _ => false
    entries.map(entry => entry.outputs.filter(patternFilter).length).sum

def secondStar(inputLines: Seq[String]): Long =
    case class MappingA(mapsTo: Char)
    case class MappingCF(mapsTo: Set[Char])
    case class MappingC(mapsTo: Char)
    case class MappingF(mapsTo: Char)
    case class MappingB(mapsTo: Char)
    case class MappingD(mapsTo: Char)
    case class MappingG(mapsTo: Char)
    case class MappingE(mapsTo: Char)

    def deduceAandCF(entry: Entry): (MappingA, MappingCF) =
        val patternOfOne = entry.patterns.filter(_.length == 2).head
        val patternOfSeven = entry.patterns.filter(_.length == 3).head
        val mappingA = MappingA((patternOfSeven.toSet &~ patternOfOne.toSet).head)
        val mappingCF = MappingCF(patternOfSeven.toSet - mappingA.mapsTo)
        (mappingA, mappingCF)

    def deduceCandF(entry: Entry, mappingCF: MappingCF): (MappingC, MappingF) =
        val patternsWithoutOneSegment = entry.patterns.filter(_.length == 6)
        val patternOfSix = patternsWithoutOneSegment.map(_.toSet)
                            .filter(pattern => (mappingCF.mapsTo & pattern).size == 1).head
        val mappingF = MappingF((mappingCF.mapsTo & patternOfSix).head)
        val mappingC = MappingC((mappingCF.mapsTo &~ Set(mappingF.mapsTo)).head)
        (mappingC, mappingF)

    def deduceBandD(entry: Entry, mappingCF: MappingCF): (MappingB, MappingD) =
        val entryOfFour = entry.patterns.filter(_.length == 4).head.toSet
        val mappingBD = entryOfFour &~ mappingCF.mapsTo
        val patternsWithoutOneSegment = entry.patterns.filter(_.length == 6)
        val patternOfZero = patternsWithoutOneSegment.map(_.toSet)
                            .filter(pattern => (mappingBD & pattern).size == 1).head
        val mappingD = MappingD((mappingBD &~ patternOfZero).head)
        val mappingB = MappingB((mappingBD - mappingD.mapsTo).head)
        (mappingB, mappingD)

    def deduceEandG(entry: Entry, mappingA: MappingA, mappingB: MappingB, mappingC: MappingC,
                    mappingD: MappingD, mappingF: MappingF): (MappingE, MappingG) =
        val mappingOfABCDF = Set(mappingA.mapsTo, mappingB.mapsTo, mappingC.mapsTo,
                                 mappingD.mapsTo, mappingF.mapsTo)
        val mappingG = MappingG((entry.patterns.filter(_.length == 6).filter(
            pattern => (pattern.toSet &~ mappingOfABCDF).size == 1).head.toSet &~ mappingOfABCDF).head)
        val mappingE = MappingE(((entry.patterns.filter(_.length == 7).head.toSet &~ mappingOfABCDF) - mappingG.mapsTo).head)
        (mappingE, mappingG)

    def patternToInt(stringPattern: String, mappingA: MappingA, mappingB: MappingB, mappingC: MappingC,
                     mappingD: MappingD, mappingE: MappingE, mappingF: MappingF, mappingG: MappingG): Int =
        def translate(pattern: Set[Char]): Set[Char] =
            pattern.map(char => char match
                case 'a' => mappingA.mapsTo
                case 'b' => mappingB.mapsTo
                case 'c' => mappingC.mapsTo
                case 'd' => mappingD.mapsTo
                case 'e' => mappingE.mapsTo
                case 'f' => mappingF.mapsTo
                case 'g' => mappingG.mapsTo)
        val pattern = stringPattern.toSet
        if pattern == translate(Set('a', 'b', 'c', 'e', 'f', 'g')) then 0
        else if pattern == translate(Set('c', 'f')) then 1
        else if pattern == translate(Set('a', 'c', 'd', 'e', 'g')) then 2
        else if pattern == translate(Set('a', 'c', 'd', 'f', 'g')) then 3
        else if pattern == translate(Set('b', 'c', 'd', 'f')) then 4
        else if pattern == translate(Set('a', 'b', 'd', 'f', 'g')) then 5
        else if pattern == translate(Set('a', 'b', 'd', 'e', 'f', 'g')) then 6
        else if pattern == translate(Set('a', 'c', 'f')) then 7
        else if pattern == translate(Set('a', 'b', 'd', 'c', 'e', 'f', 'g')) then 8
        else if pattern == translate(Set('a', 'b', 'd', 'c', 'f', 'g')) then 9
        else throw Exception(s"$pattern does not match any digit pattern.")

    def entryToInt(entry: Entry): Int =
        val (mappingA, mappingCF) = deduceAandCF(entry)
        val (mappingC, mappingF) = deduceCandF(entry, mappingCF)
        val (mappingB, mappingD) = deduceBandD(entry, mappingCF)
        val (mappingE, mappingG) = deduceEandG(entry, mappingA, mappingB, mappingC, mappingD, mappingF)
        val ints = entry.outputs.map(patternToInt(_, mappingA, mappingB, mappingC, mappingD, mappingE, mappingF, mappingG))
        val scales = Seq(1000, 100, 10, 1)
        ints.zip(scales).map((value, scale) => value * scale).sum

    val entries = inputLines.map(Entry.from).toSeq
    val (mappingA, mappingCF) = deduceAandCF(entries.head)
    val (mappingC, mappingF) = deduceCandF(entries.head, mappingCF)
    val (mappingB, mappingD) = deduceBandD(entries.head, mappingCF)
    val (mappingE, mappingG) = deduceEandG(entries.head, mappingA, mappingB, mappingC, mappingD, mappingF)
    entries.map(entryToInt).sum

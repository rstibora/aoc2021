import scala.io.Source


case class Entry(patterns: Seq[String], outputs: Seq[String])

object Entry:
    def from(entry: String): Entry =
        val splitEntry = entry.split("\\|")
        Entry(splitEntry(0).split(" "), splitEntry(1).split(" "))

@main def firstStar() =
    val entries = Source.fromFile("./inputs/day08").getLines.map(Entry.from)
    def patternFilter(pattern: String): Boolean =
        pattern.length match
            case 2 | 3 | 4 | 7 => true
            case _ => false
    println(entries.map(entry => entry.outputs.filter(patternFilter).length).sum)

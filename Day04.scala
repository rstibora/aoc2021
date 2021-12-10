import scala.io.Source
import scala.compiletime.ops.boolean

enum Field:
    case Unmarked(number: Int, x: Int, y: Int)
    case Marked(number: Int, x: Int, y: Int)

case class Board(fields: Seq[Field])

extension (board: Board)
    def draw(number: Int): Board =
        board.copy(fields = board.fields.map(
            field => field match 
                            case Field.Unmarked(fieldNumber, x, y) if fieldNumber == number => Field.Marked(number, x, y)
                            case field => field))

    def checkWinningCondition(): Boolean =
        for row <- 0 to 4 do
            if board.fields.filter(field => field match
                                                    case Field.Marked(_, _ , fieldRow) if fieldRow == row => true
                                                    case _ => false).length == 5
            then return true
        for column <- 0 to 4 do
            if board.fields.filter(field => field match
                                                    case Field.Marked(_, fieldColumn, _) if fieldColumn == column => true
                                                    case _ => false).length == 5
            then return true
        false

    def calculateWinningScore(): Int =
        board.fields.map(field => field match
                                            case Field.Unmarked(number, _, _) => number
                                            case _ => 0).sum

@main def firstStar() =
    val inputLines = Source.fromFile("./inputs/day04").getLines
    val numbersToDraw = inputLines.next.split(",").map(_.toInt)
    var boards: Seq[Board] = List()

    inputLines.next
    while inputLines.hasNext do
        var fields: Seq[Field] = List()
        for y <- 0 until 5 do
            val rowNumbers = inputLines.next.split(" ").filter(split => split != "").map(_.toInt)
            for x <- 0 until 5 do
                fields = fields :+ Field.Unmarked(rowNumbers(x), x, y)
        boards = boards :+ Board(fields)
        if inputLines.hasNext then inputLines.next

    def playUntilWin(boards: Seq[Board], numbersToDraw: Seq[Int]): Int =
        val drawnNumber = numbersToDraw.head
        val playedBoards = boards.map(_.draw(drawnNumber))
        val winningBoards = playedBoards.filter(_.checkWinningCondition())
        if !winningBoards.isEmpty then
            return winningBoards.head.calculateWinningScore() * drawnNumber
        playUntilWin(playedBoards, numbersToDraw.tail)

    println(playUntilWin(boards, numbersToDraw.toSeq))
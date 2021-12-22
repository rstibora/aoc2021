package aoc2021.day4

import scala.io.Source


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

def prepareGame(inputLines: Iterator[String]): (Seq[Board], Seq[Int]) =
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
    (boards, numbersToDraw)

def firstStar(inputLines: Seq[String]): Long =
    var (boards, numbersToDraw) = prepareGame(inputLines.iterator)

    def playUntilWin(boards: Seq[Board], numbersToDraw: Seq[Int]): Int =
        val drawnNumber = numbersToDraw.head
        val playedBoards = boards.map(_.draw(drawnNumber))
        val winningBoards = playedBoards.filter(_.checkWinningCondition())
        if !winningBoards.isEmpty then
            return winningBoards.head.calculateWinningScore() * drawnNumber
        playUntilWin(playedBoards, numbersToDraw.tail)

    playUntilWin(boards, numbersToDraw.toSeq)


def secondStar(inputLines: Seq[String]): Long =
    var (boards, numbersToDraw) = prepareGame(inputLines.iterator)

    def playUntilLastBoard(boards: Seq[Board], numbersToDraw: Seq[Int]): Int =
        val drawnNumber = numbersToDraw.head
        val playedBoards = boards.map(_.draw(drawnNumber))
        val remainingBoards = playedBoards.filter(!_.checkWinningCondition())
        if remainingBoards.length == 0 then
            return playedBoards.head.calculateWinningScore() * drawnNumber
        playUntilLastBoard(remainingBoards, numbersToDraw.tail)

    playUntilLastBoard(boards, numbersToDraw.toSeq)

package aoc2021.day2

enum Instruction:
    case Forward(amount: Int)
    case Up(amount: Int)
    case Down(amount: Int)

object Instruction:
    def from(instructionLine: String): Instruction =
        instructionLine match
            case s"forward $x" if x.toIntOption.isDefined => Forward(x.toInt)
            case s"up $x" if x.toIntOption.isDefined => Up(x.toInt)
            case s"down $x" if x.toIntOption.isDefined => Down(x.toInt)
            case _ => throw new Exception(s"$instructionLine is not a valid command.")

def firstStar(inputLines: Seq[String]): Long =
    var xPosition = 0
    var depth = 0
    for
        instruction <- inputLines.map(Instruction.from)
    do
        instruction match
            case Instruction.Forward(amount) => xPosition += amount
            case Instruction.Up(amount) => depth -= amount
            case Instruction.Down(amount) => depth += amount
    xPosition * depth

def secondStar(inputLines: Seq[String]): Long =
    var xPosition = 0
    var depth = 0
    var aim = 0
    for
        instruction <- inputLines.map(Instruction.from)
    do
        instruction match
            case Instruction.Forward(amount) =>
                xPosition += amount
                depth += amount * aim
            case Instruction.Up(amount) => aim -= amount
            case Instruction.Down(amount) => aim += amount
    xPosition * depth

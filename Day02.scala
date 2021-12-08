import scala.io.Source


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

def firstStar() =
    var xPosition = 0
    var depth = 0
    for
        instruction <- Source.fromFile("./inputs/day02").getLines.map(Instruction.from)
    do
        instruction match
            case Instruction.Forward(amount) => xPosition += amount
            case Instruction.Up(amount) => depth -= amount
            case Instruction.Down(amount) => depth += amount
    println(xPosition * depth)

@main def secondStar() =
    var xPosition = 0
    var depth = 0
    var aim = 0
    for
        instruction <- Source.fromFile("./inputs/day02").getLines.map(Instruction.from)
    do
        instruction match
            case Instruction.Forward(amount) => 
                xPosition += amount
                depth += amount * aim
            case Instruction.Up(amount) => aim -= amount
            case Instruction.Down(amount) => aim += amount
    println(xPosition * depth)

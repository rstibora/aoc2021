import scala.io.Source


enum Instruction:
    case Forward(amount: Int)
    case Up(amount: Int)
    case Down(amount: Int)

def parseInstruction(instruction: String): Instruction =
    import Instruction.*

    val amount = instruction.split(" ")(1).toInt
    return instruction.split(" ")(0) match
        case "forward" => return Forward(amount)
        case "up" => return Up(amount)
        case "down" => return Down(amount)

def firstStar() =
    import Instruction.*

    var xPosition = 0
    var depth = 0
    for
        instruction <- Source.fromFile("./inputs/day02").getLines.map(parseInstruction)
    do
        instruction match {
            case Forward(amount) => xPosition += amount
            case Up(amount) => depth -= amount
            case Down(amount) => depth += amount
        }

    println(xPosition * depth)

@main def secondStar() =
    import Instruction.*

    var xPosition = 0
    var depth = 0
    var aim = 0
    for
        instruction <- Source.fromFile("./inputs/day02").getLines.map(parseInstruction)
    do
        instruction match {
            case Forward(amount) => 
                xPosition += amount
                depth += amount * aim
            case Up(amount) => aim -= amount
            case Down(amount) => aim += amount
        }
    println(xPosition * depth)

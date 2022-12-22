package day22

import utils.Utils._

import scala.annotation.tailrec

object Main extends App {
  sealed trait Instruction

  case object TurnLeft extends Instruction

  case object TurnRight extends Instruction

  case class Walk(steps: Int) extends Instruction

  sealed trait Direction

  case object Left extends Direction

  case object Right extends Direction

  case object Up extends Direction

  case object Down extends Direction

  private def parse(input: List[String]): (Map[(Int, Int), Char], List[Instruction]) = {
    val List(rawMap, instructions) = splitAt(input, "")

    val map = rawMap.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char])({ case (acc, (curr, y)) =>
      acc ++ curr.zipWithIndex.filter(el => List('.', '#').contains(el._1)).map({ case (char, x) => (x, y) -> char }).toMap
    })

    val pattern = "^(\\d+)(.*)$".r

    @tailrec
    def parseInstructions(input: String, result: List[Instruction] = Nil): List[Instruction] = input match {
      case "" => result.reverse
      case pattern(num, rest) => parseInstructions(rest, Walk(num.toInt) :: result)
      case s"L$rest" => parseInstructions(rest, TurnLeft :: result)
      case s"R$rest" => parseInstructions(rest, TurnRight :: result)
    }

    (map, parseInstructions(instructions.head))
  }

  private def walkRight(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char]): (Int, Int) = {
    var stepsRemaining = steps
    val (posX, posY) = currentPosition
    var currentX = posX
    while (stepsRemaining > 0) {
      val nextX = currentX + 1
      val nextPosition = (nextX, posY)
      if (map.contains(nextPosition)) {
        val nextElement = map(nextPosition)
        if (nextElement == '.') {
          currentX = nextX
        } else if (nextElement == '#') {
          return (currentX, posY)
        }
      } else {
        val (nextPosition, nextElement) = map.filter({ case ((_, y), _) => y == posY }).minBy({ case ((x, _), _) => x })
        if (nextElement != '.') {
          return (currentX, posY)
        } else {
          currentX = nextPosition._1
        }
      }
      stepsRemaining -= 1
    }
    (currentX, posY)
  }

  private def walkLeft(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char]): (Int, Int) = {
    var stepsRemaining = steps
    val (posX, posY) = currentPosition
    var currentX = posX
    while (stepsRemaining > 0) {
      val nextX = currentX - 1
      val nextPosition = (nextX, posY)
      if (map.contains(nextPosition)) {
        val nextElement = map(nextPosition)
        if (nextElement == '.') {
          currentX = nextX
        } else if (nextElement == '#') {
          return (currentX, posY)
        }
      } else {
        val (nextPosition, nextElement) = map.filter({ case ((_, y), _) => y == posY }).maxBy({ case ((x, _), _) => x })
        if (nextElement != '.') {
          return (currentX, posY)
        } else {
          currentX = nextPosition._1
        }
      }
      stepsRemaining -= 1
    }
    (currentX, posY)
  }

  private def walkDown(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char]): (Int, Int) = {
    var stepsRemaining = steps
    val (posX, posY) = currentPosition
    var currentY = posY
    while (stepsRemaining > 0) {
      val nextY = currentY + 1
      val nextPosition = (posX, nextY)
      if (map.contains(nextPosition)) {
        val nextElement = map(nextPosition)
        if (nextElement == '.') {
          currentY = nextY
        } else if (nextElement == '#') {
          return (posX, currentY)
        }
      } else {
        val (nextPosition, nextElement) = map.filter({ case ((x, _), _) => x == posX }).minBy({ case ((_, y), _) => y })
        if (nextElement != '.') {
          return (posX, currentY)
        } else {
          currentY = nextPosition._2
        }
      }
      stepsRemaining -= 1
    }
    (posX, currentY)
  }

  private def walkUp(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char]): (Int, Int) = {
    var stepsRemaining = steps
    val (posX, posY) = currentPosition
    var currentY = posY
    while (stepsRemaining > 0) {
      val nextY = currentY - 1
      val nextPosition = (posX, nextY)
      if (map.contains(nextPosition)) {
        val nextElement = map(nextPosition)
        if (nextElement == '.') {
          currentY = nextY
        } else if (nextElement == '#') {
          return (posX, currentY)
        }
      } else {
        val (nextPosition, nextElement) = map.filter({ case ((x, _), _) => x == posX }).maxBy({ case ((_, y), _) => y })
        if (nextElement != '.') {
          return (posX, currentY)
        } else {
          currentY = nextPosition._2
        }
      }
      stepsRemaining -= 1
    }
    (posX, currentY)
  }

  def run1(input: List[String]): Int = {
    val (map, instructions) = parse(input)
    val startingPositionX = map.keys.filter(_._2 == 0).map(_._1).min
    val startingPosition = (startingPositionX, 0)

    val endPosition = instructions.foldLeft[((Int, Int), Direction)]((startingPosition, Right))({ case ((currentPosition, currentDirection), nextInstruction) => nextInstruction match {
      case TurnLeft =>
        val newDirection = currentDirection match {
          case Up => Left
          case Left => Down
          case Down => Right
          case Right => Up
        }
        (currentPosition, newDirection)
      case TurnRight =>
        val newDirection = currentDirection match {
          case Up => Right
          case Right => Down
          case Down => Left
          case Left => Up
        }
        (currentPosition, newDirection)
      case Walk(steps) => currentDirection match {
        case Right => (walkRight(steps, currentPosition, map), currentDirection)
        case Left => (walkLeft(steps, currentPosition, map), currentDirection)
        case Down => (walkDown(steps, currentPosition, map), currentDirection)
        case Up => (walkUp(steps, currentPosition, map), currentDirection)
      }
    }
    })

    val ((x, y), direction) = endPosition

    val directionValue = direction match {
      case Right => 0
      case Down => 1
      case Left => 2
      case Up => 3
    }

    1000 * (y + 1) + 4 * (x + 1) + directionValue
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day22-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day22-test01.txt")))
}

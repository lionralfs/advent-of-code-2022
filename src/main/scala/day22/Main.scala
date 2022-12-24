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

  private def walkRight(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char], isPart2: Boolean): ((Int, Int), Direction) = {
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
          return ((currentX, posY), Right)
        }
      } else {
        if (isPart2) {
          val (newPos, newDirection) = (currentX, posY) match {
            // leaving 3 to the right -> entering 4 from the right (walking left)
            case (149, y) if y < 50 => ((99, 149 - y), Left)
            // leaving 1 to the right -> entering 3 from the bottom (walking up)
            case (99, y) if y < 100 => ((y + 50, 49), Up)
            // leaving 4 to the right -> entering 3 from the right (walking left)
            case (99, y) => ((149, 149 - y), Left)
            // leaving 5 to the right -> entering 4 from the bottom (walking up)
            case (49, y) => ((y - 100, 149), Up)
          }
          if (map(newPos) != '.') {
            return ((currentX, posY), Right)
          } else {
            return walkInDirection(stepsRemaining - 1, newPos, map, newDirection, isPart2)
          }
        } else {
          val (nextPosition, nextElement) = map.filter({ case ((_, y), _) => y == posY }).minBy({ case ((x, _), _) => x })
          if (nextElement != '.') {
            return ((currentX, posY), Right)
          } else {
            currentX = nextPosition._1
          }
        }
      }
      stepsRemaining -= 1
    }
    ((currentX, posY), Right)
  }

  private def walkLeft(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char], isPart2: Boolean): ((Int, Int), Direction) = {
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
          return ((currentX, posY), Left)
        }
      } else {
        if (isPart2) {
          val (newPos, newDirection) = (currentX, posY) match {
            // leaving 2 to the left -> entering 6 from the left (walking right)
            case (50, y) if y < 50 => ((0, 149 - y), Right)
            // leaving 1 to the left -> entering 6 from the top (walking down)
            case (50, y) => ((y - 50, 100), Down)
            // leaving 6 to the left -> entering 2 from the left (walking right)
            case (0, y) if y < 150 => ((50, 149 - y), Right)
            // leaving 5 to the left -> entering 2 from the top (walking down)
            case (0, y) => ((y - 100, 0), Down)
          }
          if (map(newPos) != '.') {
            return ((currentX, posY), Left)
          } else {
            return walkInDirection(stepsRemaining - 1, newPos, map, newDirection, isPart2)
          }
        } else {
          val (nextPosition, nextElement) = map.filter({ case ((_, y), _) => y == posY }).maxBy({ case ((x, _), _) => x })
          if (nextElement != '.') {
            return ((currentX, posY), Left)
          } else {
            currentX = nextPosition._1
          }
        }
      }
      stepsRemaining -= 1
    }
    ((currentX, posY), Left)
  }

  private def walkDown(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char], isPart2: Boolean): ((Int, Int), Direction) = {
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
          return ((posX, currentY), Down)
        }
      } else {
        if (isPart2) {
          val (newPos, newDirection) = (posX, currentY) match {
            // leaving 3 to the bottom -> entering 1 from the right (walking left)
            case (x, 49) => ((99, x - 50), Left)
            // leaving 4 to the bottom -> entering 5 from the right (walking left)
            case (x, 149) => ((49, x + 100), Left)
            // leaving 5 to the bottom -> entering 3 from the top (walking down)
            case (x, 199) => ((x + 100, 0), Down)
          }
          if (map(newPos) != '.') {
            return ((posX, currentY), Down)
          } else {
            return walkInDirection(stepsRemaining - 1, newPos, map, newDirection, isPart2)
          }
        } else {
          val (nextPosition, nextElement) = map.filter({ case ((x, _), _) => x == posX }).minBy({ case ((_, y), _) => y })
          if (nextElement != '.') {
            return ((posX, currentY), Down)
          } else {
            currentY = nextPosition._2
          }
        }
      }
      stepsRemaining -= 1
    }
    ((posX, currentY), Down)
  }

  private def walkUp(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char], isPart2: Boolean): ((Int, Int), Direction) = {
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
          return ((posX, currentY), Up)
        }
      } else {
        if (isPart2) {
          val (newPos, newDirection) = (posX, currentY) match {
            // leaving 2 to the top -> entering 5 from the left (walking right)
            case (x, 0) if x < 100 => ((0, x + 100), Right)
            // leaving 3 to the top -> entering 5 from the bottom (walking up)
            case (x, 0) => ((x - 100, 199), Up)
            // leaving 6 to the top -> entering 1 to the left (walking right)
            case (x, 100) => ((50, x + 50), Right)
          }
          if (map(newPos) != '.') {
            return ((posX, currentY), Up)
          } else {
            return walkInDirection(stepsRemaining - 1, newPos, map, newDirection, isPart2)
          }
        } else {
          val (nextPosition, nextElement) = map.filter({ case ((x, _), _) => x == posX }).maxBy({ case ((_, y), _) => y })
          if (nextElement != '.') {
            return ((posX, currentY), Up)
          } else {
            currentY = nextPosition._2
          }
        }
      }
      stepsRemaining -= 1
    }
    ((posX, currentY), Up)
  }

  private def walkInDirection(steps: Int, currentPosition: (Int, Int), map: Map[(Int, Int), Char], direction: Direction, isPart2: Boolean): ((Int, Int), Direction) = {
    direction match {
      case Up => walkUp(steps, currentPosition, map, isPart2)
      case Down => walkDown(steps, currentPosition, map, isPart2)
      case Left => walkLeft(steps, currentPosition, map, isPart2)
      case Right => walkRight(steps, currentPosition, map, isPart2)
    }
  }

  private def calc(input: List[String], isPart2: Boolean): Int = {
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
      case Walk(steps) => walkInDirection(steps, currentPosition, map, currentDirection, isPart2)
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

  def run1(input: List[String]): Int = calc(input, isPart2 = false)

  def run2(input: List[String]): Int = calc(input, isPart2 = true)

  println("[Part1]:", run1(readFileByLine("day22-input.txt")))
  println("[Part2]:", run2(readFileByLine("day22-input.txt")))
}

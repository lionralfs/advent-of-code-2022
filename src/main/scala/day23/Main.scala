package day23

import utils.Utils._

object Main extends App {
  private def parse(input: List[String]): Set[(Int, Int)] = {
    input.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char])({ case (acc, (curr, y)) =>
      acc ++ curr.zipWithIndex.map({ case (char, x) => (x, y) -> char }).toMap
    }).filter({ case (_, char) => char == '#' }).keys.toSet
  }

  private def hasNeighbors(currentState: Set[(Int, Int)], elf: (Int, Int)): Boolean = {
    val (elfX, elfY) = elf
    val directions = List(
      (0, -1), // N
      (-1, -1), // NW
      (1, -1), // NE
      (0, 1), // S
      (-1, 1), // SW
      (1, 1), // SE
      (-1, 0), // W
      (1, 0) // E
    )
    directions.exists({ case (xDiff, yDiff) => currentState.contains((elfX + xDiff, elfY + yDiff)) })
  }

  private lazy val checks: List[List[(Int, Int)]] = List(
    List((0, -1), (-1, -1), (1, -1)), // north
    List((0, 1), (-1, 1), (1, 1)), // south
    List((-1, 0), (-1, -1), (-1, 1)), // west
    List((1, 0), (1, -1), (1, 1)), // east
  )


  private def selectNextPosition(currentState: Set[(Int, Int)], elf: (Int, Int), directionCheckOffset: Int): Option[(Int, Int)] = {
    if (!hasNeighbors(currentState, elf)) {
      return Some(elf)
    }
    val (elfX, elfY) = elf

    for (i <- checks.indices) {
      val check = checks((i + directionCheckOffset) % checks.length)
      val allFree = check.forall({ case (xDiff, yDiff) => !currentState.contains((elfX + xDiff, elfY + yDiff)) })
      if (allFree) {
        val (xDiff, yDiff) = check.head
        return Some((elfX + xDiff, elfY + yDiff))
      }
    }

    None
  }

  private def doRound(currentState: Set[(Int, Int)], directionCheckOffset: Int): Option[Set[(Int, Int)]] = {
    // a map to store the propositions where the key is the the newly proposed position
    // and the values is a set of elves that want to move there
    val acc = Map.empty[(Int, Int), Set[(Int, Int)]]
    val propositions = currentState.foldLeft(acc)({ case (proposedPositions, nextElf) =>
      val maybeNextPosition = selectNextPosition(currentState, nextElf, directionCheckOffset)
      val proposedNextPosition = maybeNextPosition.getOrElse(nextElf) // when no suitable position was found: stay
      proposedPositions.updatedWith(proposedNextPosition)({
        case Some(prev) => Some(prev + nextElf)
        case None => Some(Set(nextElf))
      })
    })

    if (propositions.forall({ case (newPos, oldPos) => oldPos.size == 1 && oldPos.head == newPos })) {
      return None
    }

    val newState = propositions.foldLeft(Set.empty[(Int, Int)])({ case (acc, (newPos, proposingElves)) =>
      if (proposingElves.size > 1) {
        acc ++ proposingElves
      } else {
        acc + newPos
      }
    })

    Some(newState)
  }

  def run1(input: List[String]): Int = {
    val elves = parse(input)

    val rounds = 10
    val finalElfPositions = (0 until rounds).foldLeft(elves)({ case (acc, round) => doRound(acc, round) match {
      case Some(newState) => newState
      case None => throw new IllegalStateException("this shouldn't happen in part 1")
    }
    })

    val xs = finalElfPositions.map(_._1)
    val ys = finalElfPositions.map(_._2)

    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max

    val xLength = (maxX - minX).abs + 1
    val yLength = (maxY - minY).abs + 1
    val area = xLength * yLength.abs
    area - finalElfPositions.size
  }

  def run2(input: List[String]): Int = {
    val elves = parse(input)

    val rounds = Int.MaxValue
    (0 until rounds).foldLeft(elves)({ case (acc, round) => doRound(acc, round) match {
      case Some(newState) => newState
      case None => return round + 1
    }
    })

    -1
  }

  println("[Part1]:", run1(readFileByLine("day23-input.txt")))
  println("[Part2]:", run2(readFileByLine("day23-input.txt")))
}

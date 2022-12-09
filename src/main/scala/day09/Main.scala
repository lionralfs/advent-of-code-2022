package day09

import utils.Utils._

import scala.collection.mutable.ListBuffer

object Main extends App {

  private def run(input: List[String], knots: Int): Int = {
    val linePattern = "(.) (\\d+)".r

    val (_, _, tailVisited) = input
      .map({ case linePattern(direction, count) => (direction, count.toInt) })
      // the state is a tuple of:
      // 1. the current position of the head
      // 2. the list of the knot positions (excluding the head), as a mutable ListBuffer
      // 3. the set of positions that the last knot has visited already
      .foldLeft(((0, 0), ListBuffer.fill(knots)((0, 0)), Set.empty[(Int, Int)]))({ case (state, (nextDirection, steps)) =>
        var (headPosition, knotPositions, tailVisited) = state

        for (_ <- 1 to steps) {
          // update the head position first
          headPosition = nextDirection match {
            case "U" => (headPosition._1, headPosition._2 - 1)
            case "D" => (headPosition._1, headPosition._2 + 1)
            case "L" => (headPosition._1 - 1, headPosition._2)
            case "R" => (headPosition._1 + 1, headPosition._2)
          }

          knotPositions.zipWithIndex.foreach({ case (knot, currentKnotIndex) =>
            val previousKnot = if (currentKnotIndex == 0) headPosition else knotPositions(currentKnotIndex - 1)

            val xDiff = previousKnot._1 - knot._1
            val yDiff = previousKnot._2 - knot._2

            // check whether both a horizontal and vertical move is required
            def needsReAdjust(dx: Int, dy: Int): Boolean = {
              dx.abs > 1 && previousKnot._2 != knot._2 || dy.abs > 1 && previousKnot._1 != knot._1
            }

            (xDiff, yDiff) match {
              // move diagonally
              case (dx, dy) if needsReAdjust(dx, dy) =>
                val moveX = dx.max(-1).min(1)
                val moveY = dy.max(-1).min(1)
                knotPositions(currentKnotIndex) = (knot._1 + moveX, knot._2 + moveY)
              case (dx, _) if dx > 1 => knotPositions(currentKnotIndex) = (knot._1 + 1, knot._2) // move right
              case (dx, _) if dx < -1 => knotPositions(currentKnotIndex) = (knot._1 - 1, knot._2) // move left
              case (_, dy) if dy > 1 => knotPositions(currentKnotIndex) = (knot._1, knot._2 + 1) // move down
              case (_, dy) if dy < -1 => knotPositions(currentKnotIndex) = (knot._1, knot._2 - 1) // move up
              case _ =>
            }
          })

          tailVisited = tailVisited + knotPositions.last
        }
        (headPosition, knotPositions, tailVisited)
      })

    tailVisited.size

  }

  def run1(input: List[String]): Int = {
    run(input, 1)
  }

  def run2(input: List[String]): Int = {
    run(input, 9)
  }

  println("[Part1]:", run1(readFileByLine("day09-input.txt")))
  println("[Part2]:", run2(readFileByLine("day09-input.txt")))
}

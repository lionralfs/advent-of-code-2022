package day17

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  case class Shape(points: List[(Int, Int)])

  case class Chamber(placedBlocks: mutable.Set[(Int, Int)] = mutable.Set.empty) {
    var maxHeight = 0

    override def toString: String = {
      (1 to maxHeight.max(6)).foldLeft(List("+-------+"))({ case (acc, currY) =>
        val placedX = placedBlocks.filter({ case (_, y) => y == currY }).map(_._1)
        val newLine = "|.......|".zipWithIndex.map({ case (char, i) => if (placedX.contains(i - 1)) '#' else char }).mkString("")
        newLine :: acc
      }).mkString("\n")
    }

    def addBlock(coordinates: Iterable[(Int, Int)]): Unit = {
      assert(coordinates.forall(coordinate => !placedBlocks.contains(coordinate)))
      maxHeight = maxHeight.max(coordinates.map(_._2).max)
      placedBlocks.addAll(coordinates)
    }

    def positionAfterFall(coordinates: Set[(Int, Int)]): Option[Set[(Int, Int)]] = {
      val potentialNewPosition = coordinates.map({ case (x, y) => (x, y - 1) })
      val hasFloorPosition = potentialNewPosition.exists { case (_, y) => y == 0 }
      for (coordinate <- potentialNewPosition) {
        if (placedBlocks.contains(coordinate)) return None
      }
      if (!hasFloorPosition) {
        Some(potentialNewPosition)
      } else {
        None
      }
    }
  }

  def run1(input: List[String]): Int = {
    val moves = input.head.split("").toList

    val shapes = List(
      Shape(points = List((0, 0), (1, 0), (2, 0), (3, 0))), // --
      Shape(points = List((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))), // +
      Shape(points = List((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))), // _|
      Shape(points = List((0, 0), (0, 1), (0, 2), (0, 3))), // |
      Shape(points = List((0, 0), (1, 0), (0, 1), (1, 1))), // o
    )

    val chamber = Chamber()
    var jetIndex = 0

    def placeNextBlock(shape: Shape): Any = {
      val maxHeight = chamber.maxHeight
      var position = shape.points.map({ case (x, y) => (x + 2, y + maxHeight + 4) }).toSet

      while (true) {
        val jetDirection = moves(jetIndex % moves.length)
        jetIndex += 1
        jetDirection match {
          case ">" =>
            val newPos = position.map({ case (x, y) => (x + 1, y) })
            if (newPos.forall({ case (x, y) => x < 7 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
          case "<" =>
            val newPos = position.map({ case (x, y) => (x - 1, y) })
            if (newPos.forall({ case (x, y) => x >= 0 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
        }
        chamber.positionAfterFall(position) match {
          case Some(pos) =>
            position = pos
          case None =>
            chamber.addBlock(position)
            return
        }
      }
    }

    var blockI = 0L
    while (blockI < 2022L) {
      val shapeToPlace = shapes((blockI % shapes.length).toInt)
      placeNextBlock(shapeToPlace)
      blockI += 1
    }

    chamber.maxHeight
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day17-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day17-test01.txt")))
}

package day14

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private def parseCave(input: List[String]): mutable.Map[(Int, Int), String] = {
    val cave = mutable.Map.empty[(Int, Int), String]

    val coordPattern = "(\\d+),(\\d+)".r
    input.foreach(line => {
      line.split(" -> ")
        .map({ case coordPattern(x, y) => (x.toInt, y.toInt) })
        .toList
        .sliding(2)
        .foreach({
          case List(start, end) =>
            val isHorizonalLine = start._2 == end._2
            if (isHorizonalLine) {
              val horizontalCoord = start._2
              val steps = (start._1 - end._1).abs
              val startCoord = start._1.min(end._1)
              for (coord <- startCoord to startCoord + steps) {
                cave.update((coord, horizontalCoord), "#")
              }
            } else {
              val verticalCoord = start._1
              val steps = (start._2 - end._2).abs
              val startCoord = start._2.min(end._2)
              for (coord <- startCoord to startCoord + steps) {
                cave.update((verticalCoord, coord), "#")
              }
            }
        })
    })

    cave
  }

  def run1(input: List[String]): Int = {
    val cave = parseCave(input)

    def placeNextSandBlock(cave: mutable.Map[(Int, Int), String], maxHeight: Int): Boolean = {
      var position = (500, 0)
      while (position._2 < maxHeight) {
        val below = (position._1, position._2 + 1)
        if (cave.contains(below)) {
          val leftBelow = (position._1 - 1, position._2 + 1)
          val rightBelow = (position._1 + 1, position._2 + 1)
          if (!cave.contains(leftBelow)) {
            position = leftBelow
          } else if (!cave.contains(rightBelow)) {
            position = rightBelow
          } else {
            cave.update(position, "o")
            return true
          }
        } else {
          position = below
        }
      }
      false
    }

    val lowestBlock = cave.keys.map(_._2).max
    while (true) {
      val shouldContinue = placeNextSandBlock(cave, lowestBlock)
      if (!shouldContinue) {
        return cave.values.count(_ == "o")
      }
    }
    0
  }

  def run2(input: List[String]): Int = {
    case class CaveWithFloor(maxHeight: Int, cave: mutable.Map[(Int, Int), String]) {
      def contains(key: (Int, Int)): Boolean = key._2 == maxHeight + 2 || cave.contains(key)

      def update(key: (Int, Int), value: String): Unit = cave.update(key, value)
    }

    val cave = parseCave(input)

    def placeNextSandBlock(cave: CaveWithFloor): Boolean = {
      var position = (500, 0)
      while (true) {
        val below = (position._1, position._2 + 1)
        if (cave.contains(below)) {
          val leftBelow = (position._1 - 1, position._2 + 1)
          val rightBelow = (position._1 + 1, position._2 + 1)
          if (!cave.contains(leftBelow)) {
            position = leftBelow
          } else if (!cave.contains(rightBelow)) {
            position = rightBelow
          } else if (position == (500, 0)) {
            cave.update(position, "o")
            return false
          } else {
            cave.update(position, "o")
            return true
          }
        } else {
          position = below
        }
      }
      false
    }

    val lowestBlock = cave.keys.map(_._2).max
    val caveWithFloor = CaveWithFloor(lowestBlock, cave)
    while (true) {
      val shouldContinue = placeNextSandBlock(caveWithFloor)
      if (!shouldContinue) {
        return cave.values.count(_ == "o")
      }
    }
    1
  }

  println("[Part1]:", run1(readFileByLine("day14-test01.txt")))
  println("[Part2]:", run2(readFileByLine("day14-test01.txt")))
}

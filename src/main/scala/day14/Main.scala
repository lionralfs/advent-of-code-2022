package day14

import utils.Utils._

import scala.collection.mutable
import scala.util.control.Breaks.break

object Main extends App {
  def run1(input: List[String]): Int = {
    val grid = mutable.Map.empty[(Int, Int), String]

    val coordPattern = "(\\d+),(\\d+)".r
    input.foreach(line => {
      line.split(" -> ")
        .map({ case coordPattern(x, y) => (x.toInt, y.toInt) })
        .toList
        .sliding(2)
        .foreach({
          case List(start, end) => {
            val isHorizonalLine = start._2 == end._2
            if (isHorizonalLine) {
              val horizontalCoord = start._2
              val steps = (start._1 - end._1).abs
              val startCoord = start._1.min(end._1)
              for (coord <- startCoord to startCoord + steps) {
                grid.update((coord, horizontalCoord), "#")
              }
            } else {
              val verticalCoord = start._1
              val steps = (start._2 - end._2).abs
              val startCoord = start._2.min(end._2)
              for (coord <- startCoord to startCoord + steps) {
                grid.update((verticalCoord, coord), "#")
              }
            }
          }
        })
    })

    println(grid)
    val lowestBlock = grid.keys.map(_._2).max
    println(lowestBlock)
    for (iteration <- 1 to 20) {
      var position = (500, 0)
      while (position._2 < lowestBlock) {
        val below = (position._1, position._2)
        println(below)
        if (!grid.contains(below)) {
          position = (position._1, position._2 + 1)
        } else {
          break
        }
      }
    }
    1
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day14-test01.txt")))
  //  println("[Part2]:", run2(readFileByLine("day14-test01.txt")))
}

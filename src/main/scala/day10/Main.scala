package day10

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private def makeMap(input: List[String]): Map[Int, Int] = {
    val (valueAfterCycles, _) = input.foldLeft((Map[Int, Int](0 -> 1), 0))({ case ((map, currentCycle), curr) =>
      curr match {
        case "noop" =>
          val newMap = map.updated(currentCycle + 1, map(currentCycle))
          (newMap, currentCycle + 1)
        case other =>
          val valueToAdd = other.split("addx ")(1).toInt
          val oldValue = map(currentCycle)
          val newValue = oldValue + valueToAdd
          val newMap = map
            .updated(currentCycle + 1, oldValue)
            .updated(currentCycle + 2, newValue)
          (newMap, currentCycle + 2)
      }
    })

    valueAfterCycles
  }

  def run1(input: List[String]): Int = {
    val valueAfterCycles = makeMap(input)

    List(20, 60, 100, 140, 180, 220).foldLeft(0)((acc, curr) => {
      acc + curr * valueAfterCycles(curr - 1)
    })
  }

  def run2(input: List[String]): String = {
    val valueAfterCycles = makeMap(input)

    val pixelMap = mutable.Map.empty[Int, Char]
    var spritePosition = 1
    for (cycle <- 1 to 240) {
      val drawPosition = cycle - 1
      val doesSpriteCoverPixel = (spritePosition - drawPosition % 40).abs <= 1
      if (doesSpriteCoverPixel) {
        pixelMap(drawPosition) = '#'
      } else {
        pixelMap(drawPosition) = '.'
      }

      spritePosition = valueAfterCycles(cycle) % 40
    }

    pixelMap.toList.sortBy(_._1).map(_._2).grouped(40).map(line => line.mkString("")).mkString("\n")
  }

  println("[Part1]:", run1(readFileByLine("day10-input.txt")))
  println("[Part2]:", "\n" + run2(readFileByLine("day10-input.txt")))
}

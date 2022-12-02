package day02

import utils.Utils._

object Main extends App {
  lazy val beats = Map[String, String](
    "A" -> "C",
    "B" -> "A",
    "C" -> "B"
  )

  lazy val mapping = Map[String, String](
    "X" -> "A",
    "Y" -> "B",
    "Z" -> "C",
  )

  lazy val points = Map[String, Int](
    "A" -> 1,
    "B" -> 2,
    "C" -> 3
  )

  def run1(input: List[String]): Int = {
    input.foldLeft(0)((acc, curr) => {
      val first :: _second :: _ = curr.split(" ").toList
      val second = mapping(_second)
      if (beats(first) == second) {
        acc + points(second)
      } else if (first == second) {
        acc + 3 + points(second)
      } else {
        acc + 6 + points(second)
      }
    })
  }

  def run2(input: List[String]): Int = {
    val loses = beats.map(pair => (pair._2, pair._1))
    input.foldLeft(0)((acc, curr) => {
      val first :: _second :: _ = curr.split(" ").toList
      val second = mapping(_second)
      second match {
        case "A" =>
          // lose
          acc + points(beats(first))
        case "B" =>
          // draw
          acc + 3 + points(first)
        case "C" =>
          // win
          val whatToPick = loses(first)
          acc + 6 + points(whatToPick)
      }
    })
  }

  println("[Part1]:", run1(readFileByLine("day02-input.txt")))
  println("[Part2]:", run2(readFileByLine("day02-input.txt")))
}
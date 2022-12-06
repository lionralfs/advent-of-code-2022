package day06

import utils.Utils._

object Main extends App {
  private def findMarker(input: String, length: Int): Int = {
    input.sliding(length).indexWhere(_.toSet.size == length) + length
  }

  def run1(input: List[String]): Int = {
    findMarker(input.head, 4)
  }

  def run2(input: List[String]): Int = {
    findMarker(input.head, 14)
  }

  println("[Part1]:", run1(readFileByLine("day06-input.txt")))
  println("[Part2]:", run2(readFileByLine("day06-input.txt")))
}

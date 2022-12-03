package day03

import utils.Utils._

object Main extends App {
  def priority(char: Char): Int = {
    val isLower = char.isLower
    char.toInt - (if (isLower) 96 else 65 - 27) // ascii table shifting
  }

  def run1(input: List[String]): Int = {
    input.foldLeft(0)((acc, curr) => {
      val l = curr.toList
      val (left, right) = l.splitAt(l.length / 2)
      val duplicate = (left.toSet & right.toSet).last
      acc + priority(duplicate)
    })
  }

  def run2(input: List[String]): Int = {
    input.grouped(3).foldLeft(0)((acc, curr) => {
      val first :: second :: third :: _ = curr
      val duplicate = (first.toSet & second.toSet & third.toSet).last
      acc + priority(duplicate)
    })
  }

  println("[Part1]:", run1(readFileByLine("day03-input.txt")))
  println("[Part2]:", run2(readFileByLine("day03-input.txt")))
}
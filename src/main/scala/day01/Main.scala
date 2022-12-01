package day01

import utils.Utils._

object Main extends App {
  def run1(input: List[String]): Int = {
    val elves = splitAt(input, "")
    elves.map(calories => calories.map(_.toInt).sum).max
  }

  def run2(input: List[String]): Int = {
    val elves = splitAt(input, "")
    elves.map(calories => calories.map(_.toInt).sum).sorted(Ordering[Int].reverse).take(3).sum
  }

  println("[Part1]:", run1(readFileByLine("day01-input.txt")))
  println("[Part2]:", run2(readFileByLine("day01-input.txt")))
}
package day04

import utils.Utils._

object Main extends App {
  def parseRange(str: String): Set[Int] = {
    val start :: end :: _ = str.split("-").map(_.toInt).toList
    (start to end).toSet
  }

  def pairsOfSections(input: List[String]): List[(Set[Int], Set[Int])] = {
    input
      .map(line => {
        line.split(",") match {
          case Array(left, right) => (parseRange(left), parseRange(right))
        }
      })
  }

  def run1(input: List[String]): Int = {
    pairsOfSections(input).count(pair => {
      val (first, second) = pair
      first.subsetOf(second) || second.subsetOf(first)
    })
  }

  def run2(input: List[String]): Int = {
    pairsOfSections(input).count(pair => {
      val (first, second) = pair
      first.exists(second.contains) || second.exists(first.contains)
    })
  }

  println("[Part1]:", run1(readFileByLine("day04-input.txt")))
  println("[Part2]:", run2(readFileByLine("day04-input.txt")))
}

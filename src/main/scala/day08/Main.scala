package day08

import utils.Utils._

object Main extends App {
  def run1(input: List[String]): Int = {
    val grid = input.map(_.split("").map(_.toInt).toList)

    val visibleCounts = for {
      (row, i) <- grid.zipWithIndex
      (item, j) <- row.zipWithIndex
    } yield {
      lazy val visibleLeft = (0 until j).toList.forall(index => grid(i)(index) < item)
      lazy val visibleTop = (0 until i).toList.forall(index => grid(index)(j) < item)
      lazy val visibleRight = (j + 1 until row.length).toList.forall(index => grid(i)(index) < item)
      lazy val visibleBottom = (i + 1 until grid.length).toList.forall(index => grid(index)(j) < item)

      if (visibleLeft || visibleTop || visibleRight || visibleBottom) {
        1
      } else {
        0
      }
    }
    visibleCounts.sum
  }

  def run2(input: List[String]): Int = {
    val grid = input.map(_.split("").map(_.toInt).toList)

    val scenicScores = for {
      (row, i) <- grid.zipWithIndex
      (item, j) <- row.zipWithIndex
    } yield {
      val maxViewLeft = (0 until j).reverse.toList.find(index => grid(i)(index) >= item) match {
        case Some(index) => j - index
        case None => j
      }
      val maxViewRight = (j + 1 until row.length).toList.find(index => grid(i)(index) >= item) match {
        case Some(index) => index - j
        case None => (row.length - 1) - j
      }
      val maxViewTop = (0 until i).reverse.toList.find(index => grid(index)(j) >= item) match {
        case Some(index) => i - index
        case None => i
      }
      val maxViewBottom = (i + 1 until grid.length).toList.find(index => grid(index)(j) >= item) match {
        case Some(index) => index - i
        case None => (grid.length - 1) - i
      }

      maxViewTop * maxViewLeft * maxViewBottom * maxViewRight
    }
    scenicScores.max
  }

  println("[Part1]:", run1(readFileByLine("day08-input.txt")))
  println("[Part2]:", run2(readFileByLine("day08-input.txt")))
}

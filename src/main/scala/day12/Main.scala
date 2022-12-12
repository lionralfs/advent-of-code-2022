package day12

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private def parse(input: List[String]) = {
    var start: (Int, Int) = null
    val field = input.zipWithIndex.map({ case (row, y) =>
      row.split("").toList.zipWithIndex.map({ case (char, x) =>
        char match {
          case "S" =>
            start = (x, y)
            96
          case "E" => 123
          case _ => char.head.toInt
        }
      })
    })
    (field, start)
  }

  private def neighbors(field: List[List[Int]], point: (Int, Int)) = {
    var neighbors = List.empty[(Int, Int)]
    val (oldX, oldY) = point

    // left
    if (oldX > 0) {
      neighbors = neighbors.appended((oldX - 1, oldY))
    }

    // right
    if (oldX < field.head.length - 1) {
      neighbors = neighbors.appended((oldX + 1, oldY))
    }

    // top
    if (oldY > 0) {
      neighbors = neighbors.appended((oldX, oldY - 1))
    }

    // bottom
    if (oldY < field.length - 1) {
      neighbors = neighbors.appended((oldX, oldY + 1))
    }

    neighbors.filter({
      case (newX, newY) =>
        val newValue = field(newY)(newX)
        val oldValue = field(point._2)(point._1)

        newValue - oldValue <= 1
    })
  }


  private def findPath(field: List[List[Int]], starts: List[(Int, Int)]): Int = {
    val todo = mutable.Queue.from(starts.map((_, 0)))
    val seen = mutable.Set.from(starts)

    while (todo.nonEmpty) {
      val (next, distanceSoFar) = todo.dequeue()
      val (headX, headY) = next
      val currentValue = field(headY)(headX)
      if (currentValue == 123) {
        return distanceSoFar
      }

      for {
        neighbor <- neighbors(field, next)
      } {
        if (!seen.contains(neighbor)) {
          seen.addOne(neighbor)
          todo.enqueue((neighbor, distanceSoFar + 1))
        }
      }
    }

    Int.MaxValue
  }

  def run1(input: List[String]): Int = {
    val (field, start) = parse(input)
    findPath(field, List(start))
  }

  def run2(input: List[String]): Int = {
    val (field, start) = parse(input)
    val as = field.zipWithIndex.foldLeft(List.empty[(Int, Int)])({ case (acc, (row, y)) =>
      val r = row.zipWithIndex.foldLeft(List.empty[(Int, Int)])({ case (innerAcc, (char, x)) =>
        if (char == 'a'.toInt) {
          (x, y) :: innerAcc
        } else {
          innerAcc
        }
      })
      acc.concat(r)
    })

    findPath(field, start :: as)
  }

  println("[Part1]:", run1(readFileByLine("day12-input.txt")))
  println("[Part2]:", run2(readFileByLine("day12-input.txt")))
}

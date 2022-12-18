package day18

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  def directions = List(
    (-1, 0, 0),
    (1, 0, 0),
    (0, -1, 0),
    (0, 1, 0),
    (0, 0, -1),
    (0, 0, 1),
  )

  private def parse(input: List[String]): Set[(Int, Int, Int)] = {
    input.foldLeft(Set.empty[(Int, Int, Int)])({ case (acc, curr) =>
      val next = curr.split(",").map(_.toInt).toList match {
        case List(a, b, c) => (a, b, c)
      }
      acc + next
    })
  }

  def run1(input: List[String]): Int = {
    val cubes = parse(input)

    cubes.foldLeft(0)({ case (acc, (x, y, z)) =>
      acc + directions.count({ case (xDir, yDir, zDir) => !cubes.contains((x + xDir, y + yDir, z + zDir)) })
    })
  }

  def run2(input: List[String]): Int = {
    val cache = mutable.Map.empty[(Int, Int, Int), Boolean]
    val cubes = parse(input)

    def canEscape(pos: (Int, Int, Int), seen: Set[(Int, Int, Int)] = Set.empty): Boolean = {
      if (cache.contains(pos)) {
        return cache(pos)
      }

      val (x, y, z) = pos

      // for every direction (x, y and z), check if there's no cube that blocks an exit
      // there's probably a better way of checking than this
      if (!cubes.exists({ case (otherX, _, _) => otherX > x })) {
        cache.update(pos, true)
        return true
      }
      if (!cubes.exists({ case (otherX, _, _) => otherX < x })) {
        cache.update(pos, true)
        return true
      }

      if (!cubes.exists({ case (_, otherY, _) => otherY > y })) {
        cache.update(pos, true)
        return true
      }
      if (!cubes.exists({ case (_, otherY, _) => otherY < y })) {
        cache.update(pos, true)
        return true
      }

      if (!cubes.exists({ case (_, _, otherZ) => otherZ > z })) {
        cache.update(pos, true)
        return true
      }
      if (!cubes.exists({ case (_, _, otherZ) => otherZ < z })) {
        cache.update(pos, true)
        return true
      }

      val nextEmptyPositionsToCheck = for {
        (xDir, yDir, zDir) <- directions
        newPos = (x + xDir, y + yDir, z + zDir) if !seen.contains(newPos) && !cubes.contains(newPos)
      } yield newPos

      nextEmptyPositionsToCheck.exists(newPos => {
        val res = canEscape(newPos, seen + pos)
        cache.update(newPos, res)
        res
      })
    }

    cubes.foldLeft(0)({ case (acc, (x, y, z)) =>
      acc + directions.count({ case (xDir, yDir, zDir) =>
        val newPos = (x + xDir, y + yDir, z + zDir)
        !cubes.contains(newPos) && canEscape(newPos)
      })
    })

  }

  println("[Part1]:", run1(readFileByLine("day18-input.txt")))
  println("[Part2]:", run2(readFileByLine("day18-input.txt")))
}

package day24

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private def parse(input: List[String]): (Map[(Int, Int), Char], Set[((Int, Int), Char)]) = {
    val map = input.zipWithIndex.foldLeft(Map.empty[(Int, Int), Char])({ case (acc, (curr, y)) =>
      acc ++ curr.zipWithIndex.map({ case (char, x) => (x, y) -> char }).toMap
    })

    val maxX = map.keys.map(_._1).max
    val maxY = map.keys.map(_._2).max

    map.updated((1, 0), 'S').updated((maxX - 1, maxY), 'E').foldLeft((Map.empty[(Int, Int), Char], Set.empty[((Int, Int), Char)]))({
      case ((map, blizzards), (coordinate, char)) => char match {
        case '>' | '<' | '^' | 'v' => (map.updated(coordinate, '.'), blizzards + ((coordinate, char)))
        case x => (map.updated(coordinate, x), blizzards)
      }
    })
  }

  private def updateBlizzards(blizzards: Set[((Int, Int), Char)], maxX: Int, maxY: Int) = {
    blizzards.map({ case ((x, y), char) =>
      val nextCoordinate = char match {
        case '>' => if (x == maxX) (1, y) else (x + 1, y)
        case '<' => if (x == 1) (maxX, y) else (x - 1, y)
        case 'v' => if (y == maxY) (x, 1) else (x, y + 1)
        case '^' => if (y == 1) (x, maxY) else (x, y - 1)
      }
      (nextCoordinate, char)
    })
  }

  private def neighbors(position: (Int, Int), map: Map[(Int, Int), Char], blocked: Set[(Int, Int)]) = {
    val (x, y) = position
    val moves = List(
      (0, 0), // stay
      (0, -1), // up
      (0, 1), // down
      (-1, 0), // left
      (1, 0), // right
    )

    moves.map({ case (xDiff, yDiff) => (x + xDiff, y + yDiff) })
      .filter(position => map.contains(position) && map(position) != '#')
      .filterNot(blocked.contains)
  }

  private def shortestPath(
                            start: (Int, Int),
                            end: (Int, Int),
                            endSymbol: Char,
                            maxX: Int,
                            maxY: Int,
                            map: Map[(Int, Int), Char],
                            blizzards: Set[((Int, Int), Char)]
                          ): (Int, Set[((Int, Int), Char)]) = {

    def manhattan(state: ((Int, Int), Int, Set[((Int, Int), Char)])) = state._2 + (end._1 - state._1._1).abs + (end._2 - state._1._2).abs

    val todo = mutable.PriorityQueue((start, 0, blizzards))(Ordering.by(manhattan).reverse)
    val seen = mutable.Set((start, blizzards))

    while (todo.nonEmpty) {
      val (next, distanceSoFar, blizzards) = todo.dequeue()
      val currentValue = map(next)
      if (currentValue == endSymbol) {
        return (distanceSoFar, blizzards)
      }

      val nextBlizzards = updateBlizzards(blizzards, maxX, maxY)

      for {
        neighbor <- neighbors(next, map, nextBlizzards.map(_._1))
      } {
        if (!seen.contains(neighbor, nextBlizzards)) {
          seen += ((neighbor, nextBlizzards))
          todo.enqueue((neighbor, distanceSoFar + 1, nextBlizzards))
        }
      }
    }

    (-1, Set.empty)
  }

  def run1(input: List[String]): Int = {
    val (map, blizzards) = parse(input)

    val maxX = map.keys.map(_._1).max - 1
    val maxY = map.keys.map(_._2).max - 1
    val start = (1, 0)
    val end = (maxX, maxY + 1)

    val (distance, _) = shortestPath(start, end, 'E', maxX, maxY, map, blizzards)
    distance
  }

  def run2(input: List[String]): Int = {
    val (map, blizzards) = parse(input)

    val maxX = map.keys.map(_._1).max - 1
    val maxY = map.keys.map(_._2).max - 1
    val start = (1, 0)
    val end = (maxX, maxY + 1)

    val (distanceForward, blizzardsAfter) = shortestPath(start, end, 'E', maxX, maxY, map, blizzards)
    val (distanceBackward, blizzardsNew) = shortestPath(end, start, 'S', maxX, maxY, map, blizzardsAfter)
    val (distanceFinal, _) = shortestPath(start, end, 'E', maxX, maxY, map, blizzardsNew)
    distanceForward + distanceBackward + distanceFinal
  }

  println("[Part1]:", run1(readFileByLine("day24-input.txt")))
  println("[Part2]:", run2(readFileByLine("day24-input.txt")))
}

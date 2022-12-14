package day17

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private lazy val shapes = List(
    Shape(points = List((0, 0), (1, 0), (2, 0), (3, 0))), // --
    Shape(points = List((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))), // +
    Shape(points = List((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))), // _|
    Shape(points = List((0, 0), (0, 1), (0, 2), (0, 3))), // |
    Shape(points = List((0, 0), (1, 0), (0, 1), (1, 1))), // o
  )

  case class Shape(points: List[(Int, Int)])

  case class Chamber(placedBlocks: mutable.Set[(Int, Int)] = mutable.Set.empty) {
    var maxHeight = 0

    override def toString: String = {
      (1 to maxHeight.max(6)).foldLeft(List("+-------+"))({ case (acc, currY) =>
        val placedX = placedBlocks.filter({ case (_, y) => y == currY }).map(_._1)
        val newLine = "|.......|".zipWithIndex.map({ case (char, i) => if (placedX.contains(i - 1)) '#' else char }).mkString("")
        newLine :: acc
      }).mkString("\n")
    }

    def addBlock(coordinates: Iterable[(Int, Int)]): Unit = {
      assert(coordinates.forall(coordinate => !placedBlocks.contains(coordinate)))
      maxHeight = maxHeight.max(coordinates.map(_._2).max)
      placedBlocks.addAll(coordinates)
    }

    def positionAfterFall(coordinates: Set[(Int, Int)]): Option[Set[(Int, Int)]] = {
      val potentialNewPosition = coordinates.map({ case (x, y) => (x, y - 1) })
      val hasFloorPosition = potentialNewPosition.exists { case (_, y) => y == 0 }
      for (coordinate <- potentialNewPosition) {
        if (placedBlocks.contains(coordinate)) return None
      }
      if (!hasFloorPosition) {
        Some(potentialNewPosition)
      } else {
        None
      }
    }
  }

  def run1(input: List[String]): Int = {
    val moves = input.head.split("").toList

    val chamber = Chamber()
    var jetIndex = 0

    def placeNextBlock(shape: Shape): Any = {
      val maxHeight = chamber.maxHeight
      var position = shape.points.map({ case (x, y) => (x + 2, y + maxHeight + 4) }).toSet

      while (true) {
        val jetDirection = moves(jetIndex % moves.length)
        jetIndex += 1
        jetDirection match {
          case ">" =>
            val newPos = position.map({ case (x, y) => (x + 1, y) })
            if (newPos.forall({ case (x, y) => x < 7 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
          case "<" =>
            val newPos = position.map({ case (x, y) => (x - 1, y) })
            if (newPos.forall({ case (x, y) => x >= 0 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
        }
        chamber.positionAfterFall(position) match {
          case Some(pos) =>
            position = pos
          case None =>
            chamber.addBlock(position)
            return
        }
      }
    }

    var blockIndex = 0L
    while (blockIndex < 2022L) {
      val shapeToPlace = shapes((blockIndex % shapes.length).toInt)
      placeNextBlock(shapeToPlace)
      blockIndex += 1
    }

    chamber.maxHeight
  }

  def run2(input: List[String]): Long = {
    val moves = input.head.split("").toList

    var jetIndex = 0

    def placeNextBlock(chamber: Chamber, shape: Shape): Set[(Int, Int)] = {
      val maxHeight = chamber.maxHeight
      var position = shape.points.map({ case (x, y) => (x + 2, y + maxHeight + 4) }).toSet

      while (true) {
        val jetDirection = moves(jetIndex)
        jetIndex = (jetIndex + 1) % moves.length
        jetDirection match {
          case ">" =>
            val newPos = position.map({ case (x, y) => (x + 1, y) })
            if (newPos.forall({ case (x, y) => x < 7 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
          case "<" =>
            val newPos = position.map({ case (x, y) => (x - 1, y) })
            if (newPos.forall({ case (x, y) => x >= 0 && !chamber.placedBlocks.contains((x, y)) })) {
              position = newPos
            }
        }
        chamber.positionAfterFall(position) match {
          case Some(pos) =>
            position = pos
          case None =>
            chamber.addBlock(position)
            return position
        }
      }

      Set.empty
    }

    // I solved it by hand by manually finding the loop and then doing some math:
    //    240 blocks before loop
    //    loop has height 2681
    //    loop has 1980 - 240 = 1740 blocks
    //
    //    1000000000000 = 574712643 * 1740 + 1180
    //    574712643 loops are required, and 1180 steps beginning + end
    //
    //    1180 blocks = 1799 height
    //    total height = 1799 + (574712643 * 2681) = 1540804597682

    var blockCounter = 0L
    val history = mutable.Map.empty[(Int, Int, Set[Int]), (Int, Long)]
    val chamber = Chamber()
    while (true) {
      val heightBeforePlacing = chamber.maxHeight
      val blockIndex = (blockCounter % shapes.length).toInt
      val shapeToPlace = shapes(blockIndex)
      val jetIndexBefore = jetIndex
      val newPos = placeNextBlock(chamber, shapeToPlace)
      assert(newPos.nonEmpty)
      val xCoordinates = newPos.map({ case (x, _) => x })
      val historyKey = (blockIndex, jetIndex, xCoordinates)
      if (history.contains(historyKey)) { // FIXME: apparently, this check alone is not enough to find the loop
        val (heightBeforeLoopStarts, blocksBeforeLoopStarts) = history(historyKey)
        val loopHeight = heightBeforePlacing - heightBeforeLoopStarts
        val loopBlockCount = blockCounter - blocksBeforeLoopStarts
        println(s"loop starts at height $heightBeforeLoopStarts and has height: $loopHeight")
        val blocksRemaining = 1000000000000L - blocksBeforeLoopStarts
        val loops = blocksRemaining / loopBlockCount
        val remainingAfterLoops = 1000000000000L - ((loopBlockCount * loops) + blocksBeforeLoopStarts)
        val newChamber = Chamber()
        jetIndex = jetIndexBefore
        for (_ <- 1 to remainingAfterLoops.toInt) {
          val blockIndex = (blockCounter % shapes.length).toInt
          val shapeToPlace = shapes(blockIndex)
          placeNextBlock(newChamber, shapeToPlace)
          blockCounter += 1
        }
        // FIXME: just simulate the start + the end in one go
        return heightBeforeLoopStarts + (loops * loopHeight) + newChamber.maxHeight
      }
      history.update(historyKey, (heightBeforePlacing, blockCounter))
      blockCounter += 1
    }

    chamber.maxHeight
  }

  println("[Part1]:", run1(readFileByLine("day17-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day17-input.txt")))
}

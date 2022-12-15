package day15

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  private def parse(input: List[String]): List[(Int, Int, Int, Int)] = {
    input.map({
      case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
        (sensorX.toInt, sensorY.toInt, beaconX.toInt, beaconY.toInt)
    })
  }

  def run1(input: List[String], targetRow: Int): Int = {
    val lines = parse(input)
    val (coords, beacons) = lines.foldLeft(Set.empty[(Int, Int)], Set.empty[(Int, Int)])(
      { case ((accCoords, beacons), (sensorX, sensorY, beaconX, beaconY)) =>
        val yDiff = (sensorY - beaconY).abs
        if (sensorY + yDiff >= targetRow || sensorY - yDiff <= targetRow) {
          val manhattan = (sensorX - beaconX).abs + yDiff
          val yDiffToTarget = (sensorY - targetRow).abs
          val xDiffToTarget = manhattan - yDiffToTarget
          val coords = for (i <- sensorX - xDiffToTarget to sensorX + xDiffToTarget) yield (i, targetRow)
          (accCoords ++ coords, beacons.incl((beaconX, beaconY)))
        } else {
          (accCoords, beacons.incl((beaconX, beaconY)))
        }
      })

    val res = coords -- beacons
    res.size
  }

  def run2(input: List[String], maxSize: Int): Long = {
    val min = 0
    val max = maxSize
    val lines = parse(input)

    case class CustomRange(start: Int, end: Int) {
      def combine(other: CustomRange): Option[CustomRange] = {
        if (start <= other.start && end >= other.end) { // this contains other
          Some(this)
        } else if (other.start <= start && other.end >= end) { // other contains this
          Some(other)
        } else if (other.start >= start && other.start <= end) { // overlap where this starts before other
          Some(CustomRange(start, end.max(other.end)))
        } else if (other.start <= start && other.end >= start) { // overlap where other starts before this
          Some(CustomRange(other.start, end.max(other.end)))
        } else if (other.start == end + 1) { // other immediately follows this
          Some(CustomRange(start, other.end))
        } else if (start == other.end + 1) { // this immediately follows other
          Some(CustomRange(other.start, end))
        } else {
          None
        }
      }
    }

    val map = mutable.Map.empty[Int, mutable.ListBuffer[CustomRange]]

    lines.foreach {
      case (sensorX, sensorY, beaconX, beaconY) =>
        val xDiff = (sensorX - beaconX).abs
        val yDiff = (sensorY - beaconY).abs
        val manhattan = xDiff + yDiff

        for (y <- (sensorY - manhattan).max(min) to (sensorY + manhattan).min(max)) {
          val distance = (sensorY - y).abs
          val xLength = manhattan - distance
          val range = CustomRange((sensorX - xLength).max(min), (sensorX + xLength).min(max))
          map.updateWith(y) {
            case Some(old) => Some(old.append(range))
            case None => Some(mutable.ListBuffer(range))
          }
        }
    }

    def tryToCombine(ranges: mutable.ListBuffer[CustomRange]): Boolean = {
      for {
        i <- ranges.indices
        j <- i + 1 until ranges.length
      } {
        val rangeA = ranges(i)
        val rangeB = ranges(j)

        rangeA.combine(rangeB) match {
          case Some(combined) =>
            ranges.filterInPlace(e => e != rangeA && e != rangeB)
            ranges.append(combined)
            return true
          case None =>
        }
      }

      false
    }

    def findEmptySpace(): (Int, Int) = {
      map.foreach {
        case (y, ranges) =>
          while (ranges.length > 1) {
            val didCombine = tryToCombine(ranges)
            if (!didCombine && ranges.length == 2) {
              // assuming the empty spot is not at the start or finish
              return (ranges.map(_.end).min + 1, y)
            }
          }
      }

      (0, 0)
    }

    val (x, y) = findEmptySpace()
    x.toLong * 4000000 + y.toLong
  }

  println("[Part1]:", run1(readFileByLine("day15-input.txt"), targetRow = 2000000))
  println("[Part2]:", run2(readFileByLine("day15-input.txt"), maxSize = 4000000))
}

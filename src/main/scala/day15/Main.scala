package day15

import utils.Utils._

object Main extends App {
  private def parse(input: List[String]) = {
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

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day15-input.txt"), targetRow = 2000000))
  //  println("[Part2]:", run2(readFileByLine("day15-test01.txt")))
}

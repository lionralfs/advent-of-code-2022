package day16

import utils.Utils._

import scala.collection.mutable

object Main extends App {
  case class Valve(identifier: String, flowRate: Int, nextValveIdentifiers: Set[String])

  private def parse(input: List[String]) = {
    var identifierToValve = Map.empty[String, Valve]
    val nextPattern = "tunnels? leads? to valves? (.*)".r
    input.foreach {
      case s"Valve $identifier has flow rate=$flowRate; $rest" => rest match {
        case nextPattern(next) =>
          val valve = Valve(identifier, flowRate.toInt, next.split(", ").toSet)
          identifierToValve = identifierToValve.updated(identifier, valve)
      }
    }

    (identifierToValve.map { case (key, _) => (key, shortestPathToAllOthers(identifierToValve, key)) }, identifierToValve)
  }

  private def shortestPathToAllOthers(identifierToValue: Map[String, Valve], from: String) = {
    var distances = Map(from -> 0)
    val todo = mutable.Queue(from)

    while (todo.nonEmpty) {
      val first = todo.dequeue()
      val valve = identifierToValue(first)
      val neighbors = for {
        neighbor <- valve.nextValveIdentifiers if !distances.contains(neighbor)
      } yield {
        distances = distances.updated(neighbor, distances(first) + 1)
        neighbor
      }
      todo.addAll(neighbors)
    }

    distances
  }

  def run1(input: List[String]): Int = {
    val (distances, identifierToValve) = parse(input)

    val openableValves = identifierToValve.values.filter(_.flowRate > 0).map(_.identifier).toSet

    def findBest(currentPosition: String, openable: Set[String], minutesRemaining: Int, releasedPressure: Int): Int = {
      val moves = for {
        next <- openable
        stepsToMoveThereAndOpen = distances(currentPosition)(next) + 1
        minutesRemainingAfterMove = minutesRemaining - stepsToMoveThereAndOpen if minutesRemainingAfterMove > 0
        howMuchThisValveReleasesUntilDone = minutesRemainingAfterMove * identifierToValve(next).flowRate
      } yield {
        findBest(next, openable - next, minutesRemainingAfterMove, releasedPressure + howMuchThisValveReleasesUntilDone)
      }
      (moves + releasedPressure).max
    }


    findBest("AA", openableValves, 30, 0)
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day16-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day16-test01.txt")))
}

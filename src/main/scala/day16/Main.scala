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
    val (distances, identifierToValve) = parse(input)

    val openableValves = identifierToValve.values.filter(_.flowRate > 0).map(_.identifier).toSet

    // use a shared state object of the best solution so far
    val bestSeen = mutable.Map.empty[Set[String], Int]

    def findBest(
                  currentHumanPosition: String,
                  currentElephantPosition: String,
                  openable: Set[String],
                  humanMinutesRemaining: Int,
                  elephantMinutesRemaining: Int,
                  releasedPressure: Int
                ): Unit = {
      if (bestSeen.contains(openable) && bestSeen(openable) >= releasedPressure) {
        return
      } else {
        bestSeen.update(openable, releasedPressure)
      }

      // let the human walk
      for {
        next <- openable
        stepsToMoveThereAndOpen = distances(currentHumanPosition)(next) + 1
        minutesRemainingAfterMove = humanMinutesRemaining - stepsToMoveThereAndOpen if minutesRemainingAfterMove > 0
        howMuchThisValveReleasesUntilDone = minutesRemainingAfterMove * identifierToValve(next).flowRate
      } {
        findBest(next, currentElephantPosition, openable - next, minutesRemainingAfterMove, elephantMinutesRemaining, releasedPressure + howMuchThisValveReleasesUntilDone)
      }

      // let the elephant walk, it'll hit the same valves that the human already opened but they'll be skipped
      // NOTE: somehow, this works for the real input, but fails for the test input
      for {
        next <- openable
        stepsToMoveThereAndOpen = distances(currentElephantPosition)(next) + 1
        minutesRemainingAfterMove = elephantMinutesRemaining - stepsToMoveThereAndOpen if minutesRemainingAfterMove > 0
        howMuchThisValveReleasesUntilDone = minutesRemainingAfterMove * identifierToValve(next).flowRate
      } {
        findBest(currentHumanPosition, next, openable - next, humanMinutesRemaining, minutesRemainingAfterMove, releasedPressure + howMuchThisValveReleasesUntilDone)
      }
    }


    findBest("AA", "AA", openableValves, 26, 26, 0)

    bestSeen.values.max
  }

  println("[Part1]:", run1(readFileByLine("day16-input.txt")))
  println("[Part2]:", run2(readFileByLine("day16-input.txt")))
}

package day19

import utils.Utils._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {
  private case class Blueprint(
                                number: Int,
                                oreRobotOreCost: Int,
                                clayRobotOreCost: Int,
                                obsidianRobotOreCost: Int,
                                obsidianRobotClayCost: Int,
                                geodeRobotOreCost: Int,
                                geodeRobotObsidianCost: Int
                              )

  private def parse(input: List[String]): List[Blueprint] = {
    input.map {
      case s"Blueprint $n: Each ore robot costs $oo ore. Each clay robot costs $oc ore. Each obsidian robot costs $oob ore and $cob clay. Each geode robot costs $og ore and $obg obsidian." =>
        Blueprint(
          number = n.toInt,
          oreRobotOreCost = oo.toInt,
          clayRobotOreCost = oc.toInt,
          obsidianRobotOreCost = oob.toInt,
          obsidianRobotClayCost = cob.toInt,
          geodeRobotOreCost = og.toInt,
          geodeRobotObsidianCost = obg.toInt
        )
    }
  }

  private def findMaxWithCache(blueprint: Blueprint, minutes: Int): Int = {
    val cache = mutable.Map.empty[(Int, Int, Int, Int, Int, Int, Int, Int, Int), Int]

    def findMax(blueprint: Blueprint, state: (Int, Int, Int, Int, Int, Int, Int, Int, Int), maxSoFar: Int): Int = {
      val (ore, clay, obsidian, geode, oreRobots, clayRobots, obsidianRobots, geodeRobots, minutesRemaining) = state
      if (minutesRemaining == 0) {
        return geode
      }

      if (cache.contains(state)) {
        return cache(state)
      }

      if (geode + (geodeRobots + minutesRemaining) * minutesRemaining < maxSoFar) {
        return maxSoFar
      }

      val doNothing = (ore + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots, oreRobots, clayRobots, obsidianRobots, geodeRobots, minutesRemaining - 1)
      val nextStates = mutable.ListBuffer[(Int, Int, Int, Int, Int, Int, Int, Int, Int)](doNothing)

      var canMakeGeodeRobot = false

      if (ore - blueprint.geodeRobotOreCost >= 0 && obsidian - blueprint.geodeRobotObsidianCost >= 0) {
        canMakeGeodeRobot = true
        nextStates.append((ore - blueprint.geodeRobotOreCost + oreRobots, clay + clayRobots, obsidian - blueprint.geodeRobotObsidianCost + obsidianRobots, geode + geodeRobots, oreRobots, clayRobots, obsidianRobots, geodeRobots + 1, minutesRemaining - 1))
      }

      if (!canMakeGeodeRobot && ore - blueprint.oreRobotOreCost >= 0 && oreRobots < blueprint.clayRobotOreCost.max(blueprint.obsidianRobotOreCost).max(blueprint.geodeRobotOreCost)) {
        nextStates.append((ore - blueprint.oreRobotOreCost + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots, oreRobots + 1, clayRobots, obsidianRobots, geodeRobots, minutesRemaining - 1))
      }

      if (!canMakeGeodeRobot && ore - blueprint.clayRobotOreCost >= 0 && clayRobots < blueprint.obsidianRobotClayCost) {
        nextStates.append((ore - blueprint.clayRobotOreCost + oreRobots, clay + clayRobots, obsidian + obsidianRobots, geode + geodeRobots, oreRobots, clayRobots + 1, obsidianRobots, geodeRobots, minutesRemaining - 1))
      }

      if (!canMakeGeodeRobot && ore - blueprint.obsidianRobotOreCost >= 0 && clay - blueprint.obsidianRobotClayCost >= 0 && obsidianRobots < blueprint.geodeRobotObsidianCost) {
        nextStates.append((ore - blueprint.obsidianRobotOreCost + oreRobots, clay - blueprint.obsidianRobotClayCost + clayRobots, obsidian + obsidianRobots, geode + geodeRobots, oreRobots, clayRobots, obsidianRobots + 1, geodeRobots, minutesRemaining - 1))
      }

      val max = nextStates.foldLeft(maxSoFar)({ (acc, nextState) => {
        val maxNext = findMax(blueprint, nextState, acc)
        acc.max(maxNext)
      }
      })

      cache.update(state, max)
      max
    }

    val startState = (
      0, // ore
      0, // clay
      0, // obsidian
      0, // geode
      1, // ore robots
      0, // clay robots
      0, // obsidian robots
      0, // geode robots
      minutes // minutes remaining
    )
    findMax(blueprint, startState, 0)
  }

  def run1(input: List[String]): Int = {
    val blueprints = parse(input)

    // some parallel work
    val futures = for {
      blueprint <- blueprints
    } yield Future(findMaxWithCache(blueprint, 24) * blueprint.number)

    val results = Await.result(Future.sequence(futures), Duration.Inf)

    results.sum
  }

  def run2(input: List[String]): Int = {
    val blueprints = parse(input)

    // some parallel work
    val futures = for {
      blueprint <- blueprints.take(3)
    } yield Future(findMaxWithCache(blueprint, 32))

    val results = Await.result(Future.sequence(futures), Duration.Inf)

    results.product
  }

  println("[Part1]:", run1(readFileByLine("day19-input.txt")))
  println("[Part2]:", run2(readFileByLine("day19-input.txt")))
}

package day11

import utils.Utils._

import scala.collection.mutable.{ListBuffer, Map => MutableMap}

object Main extends App {
  case class Item(
                   var currentValue: Int, // only relevant for part 1
                   remainders: MutableMap[Int, Int] = MutableMap.empty // only relevant for part 2
                 )

  case class Monkey(
                     monkeyNumber: Int,
                     var items: ListBuffer[Item],
                     operation: Int => Int,
                     testSuccess: Int,
                     testFailure: Int
                   ) {
    var inspections = 0

    def inspectPart1(item: Item, monkeyDividesBy: MutableMap[Int, Int]): Int = {
      inspections += 1
      item.currentValue = operation(item.currentValue) / 3
      val isDivisible = item.currentValue % monkeyDividesBy(monkeyNumber) == 0
      if (isDivisible) testSuccess else testFailure
    }

    def inspectPart2(item: Item, monkeyDividesBy: MutableMap[Int, Int]): Int = {
      inspections += 1
      item.remainders.foreach({ case (monkeyNumber, remainder) =>
        // the main idea on how to solve part 2 is in the following line: after the operation, update the remainder for every monkey
        item.remainders.update(monkeyNumber, operation(remainder) % monkeyDividesBy(monkeyNumber))
      })
      val isDivisible = item.remainders(monkeyNumber) % monkeyDividesBy(monkeyNumber) == 0
      if (isDivisible) testSuccess else testFailure
    }
  }

  private def parse(input: List[String], divideBy3: Boolean): (List[Monkey], MutableMap[Int, Int]) = {
    val monkeyDividesBy = MutableMap.empty[Int, Int]

    val monkeys = splitAt(input, "").map(monkeyLines => {
      val monkeyNumber = monkeyLines.head match {
        case s"Monkey $n:" => n.toInt
      }
      val items = monkeyLines(1) match {
        case s"  Starting items: $rest" => rest.split(", ").map(e => Item(e.toInt)).to(ListBuffer)
      }
      val operation = monkeyLines(2) match {
        case s"  Operation: new = $op" =>
          op match {
            case s"old + old" => (old: Int) => old + old
            case s"old + $d" => (old: Int) => old + d.toInt
            case s"old * old" => (old: Int) => old * old
            case s"old * $d" => (old: Int) => old * d.toInt
          }
      }
      monkeyLines(3) match {
        case s"  Test: divisible by $d" => monkeyDividesBy.update(monkeyNumber, d.toInt)
      }
      val testSuccess = monkeyLines(4) match {
        case s"    If true: throw to monkey $d" => d.toInt
      }
      val testFailure = monkeyLines(5) match {
        case s"    If false: throw to monkey $d" => d.toInt
      }
      Monkey(monkeyNumber, items, operation, testSuccess, testFailure)
    })

    (monkeys, monkeyDividesBy)
  }


  def run1(input: List[String]): Long = {
    val (monkeys, monkeyDividesBy) = parse(input, divideBy3 = true)

    for (_ <- 1 to 20) {
      monkeys.foreach(monkey => {
        while (monkey.items.nonEmpty) {
          val first = monkey.items.head
          monkey.items = monkey.items.tail
          val newMonkey = monkey.inspectPart1(first, monkeyDividesBy)
          monkeys(newMonkey).items.append(first)
        }
      })
    }
    monkeys.map(_.inspections.toLong).sorted(Ordering[Long].reverse).take(2).product
  }

  def run2(input: List[String]): Long = {
    val (monkeys, monkeyDividesBy) = parse(input, divideBy3 = false)

    // make the "remainders map" for each item
    monkeys.foreach(monkey => {
      monkey.items.foreach(item => {
        monkeyDividesBy.foreach({ case (otherMonkey, dividesBy) =>
          item.remainders.update(otherMonkey, item.currentValue % dividesBy)
        })
      })
    })

    for (_ <- 1 to 10000) {
      monkeys.foreach(monkey => {
        while (monkey.items.nonEmpty) {
          val first = monkey.items.head
          monkey.items = monkey.items.tail
          val newMonkey = monkey.inspectPart2(first, monkeyDividesBy)
          monkeys(newMonkey).items.append(first)
        }
      })
    }
    monkeys.map(_.inspections.toLong).sorted(Ordering[Long].reverse).take(2).product
  }

  println("[Part1]:", run1(readFileByLine("day11-input.txt")))
  println("[Part2]:", run2(readFileByLine("day11-input.txt")))
}

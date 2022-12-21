package day21

import utils.Utils._

object Main extends App {
  private def parse(input: List[String]): Map[String, ((Long, Long) => Long, List[String])] = {
    input.map {
      case s"$name: $arg1 + $arg2" => (name, ((add1: Long, add2: Long) => add1 + add2, List(arg1, arg2)))
      case s"$name: $arg1 - $arg2" => (name, ((add1: Long, add2: Long) => add1 - add2, List(arg1, arg2)))
      case s"$name: $arg1 * $arg2" => (name, ((add1: Long, add2: Long) => add1 * add2, List(arg1, arg2)))
      case s"$name: $arg1 / $arg2" => (name, ((add1: Long, add2: Long) => add1 / add2, List(arg1, arg2)))
      case s"$name: $n" => (name, ((_: Long, _: Long) => n.toLong, Nil))
    }.toMap
  }

  private def calc(monkey: String, rules: Map[String, ((Long, Long) => Long, List[String])]): Long = {
    val (math, dependencies) = rules(monkey)
    if (dependencies.isEmpty) {
      math(0, 0)
    } else {
      val List(dep1, dep2) = dependencies
      math(calc(dep1, rules), calc(dep2, rules))
    }
  }

  def run1(input: List[String]): Long = {
    val monkeys = parse(input)

    calc("root", monkeys)
  }

  def run2(input: List[String]): Long = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day21-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day21-test01.txt")))
}

package day21

import utils.Utils._

import scala.annotation.tailrec
import scala.collection.mutable

object Main extends App {
  sealed trait Node {
    val left: String
    val right: String
  }

  private case class OperationNode(operator: String, left: String, right: String) extends Node

  private case class ValueNode(value: Long) extends Node {
    override val left: String = ""
    override val right: String = ""
  }

  private def parse(input: List[String]): Map[String, Node] = {
    input.map {
      case s"$name: $arg1 $operation $arg2" => (name, OperationNode(operation, arg1, arg2))
      case s"$name: $n" => (name, ValueNode(n.toLong))
    }.toMap
  }

  private def calc(monkey: String, rules: Map[String, Node]): Long = {
    rules(monkey) match {
      case ValueNode(value) => value
      case OperationNode(operator, left, right) => operator match {
        case "+" => calc(left, rules) + calc(right, rules)
        case "-" => calc(left, rules) - calc(right, rules)
        case "*" => calc(left, rules) * calc(right, rules)
        case "/" => calc(left, rules) / calc(right, rules)
      }
    }
  }

  def run1(input: List[String]): Long = {
    val monkeys = parse(input)

    calc("root", monkeys)
  }

  def run2(input: List[String]): Long = {
    val monkeys = parse(input)
    val hasHuman = mutable.Map.empty[String, Boolean]

    def calcAndFindHuman(monkey: String): (Long, Boolean) = {
      val result = monkeys(monkey) match {
        case ValueNode(value) => (value, monkey == "humn")
        case OperationNode(operator, left, right) =>
          val (leftResult, hasLeftHuman) = calcAndFindHuman(left)
          val (rightResult, hasRightHuman) = calcAndFindHuman(right)
          val hasHuman = hasLeftHuman || hasRightHuman
          operator match {
            case "+" => (leftResult + rightResult, hasHuman)
            case "-" => (leftResult - rightResult, hasHuman)
            case "*" => (leftResult * rightResult, hasHuman)
            case "/" => (leftResult / rightResult, hasHuman)
          }
      }

      hasHuman.update(monkey, result._2)
      result
    }

    @tailrec
    def calcReverse(monkey: String, target: Long): Long = {
      monkeys(monkey) match {
        case ValueNode(value) => if (monkey == "humn") target else value
        case OperationNode(operator, left, right) =>
          if (hasHuman(left)) {
            val rightResult = calc(right, monkeys)
            operator match {
              case "+" => calcReverse(left, target - rightResult)
              case "-" => calcReverse(left, target + rightResult)
              case "*" => calcReverse(left, target / rightResult)
              case "/" => calcReverse(left, target * rightResult)
            }
          } else if (hasHuman(right)) {
            val leftResult = calc(left, monkeys)
            operator match {
              case "+" => calcReverse(right, target - leftResult)
              case "-" => calcReverse(right, leftResult - target)
              case "*" => calcReverse(right, target / leftResult)
              case "/" => calcReverse(right, leftResult / target)
            }
          } else {
            throw new IllegalStateException()
          }
      }
    }

    val rootNode = monkeys("root")
    val (leftResult, isHumanLeft) = calcAndFindHuman(rootNode.left)
    val (rightResult, _) = calcAndFindHuman(rootNode.right)

    if (isHumanLeft) {
      calcReverse(rootNode.left, rightResult)
    } else {
      calcReverse(rootNode.right, leftResult)
    }
  }

  println("[Part1]:", run1(readFileByLine("day21-input.txt")))
  println("[Part2]:", run2(readFileByLine("day21-input.txt")))
}

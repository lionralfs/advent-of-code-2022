package day13

import utils.Utils._

import scala.collection.mutable.ListBuffer

object Main extends App {
  private trait Node

  private case class ListNode(nodes: ListBuffer[Node]) extends Node {
    override def toString: String = s"[${nodes.map(_.toString).mkString(",")}]"
  }

  private case class ValueNode(value: Int) extends Node {
    override def toString: String = value.toString
  }

  // custom badly-handwritten parser for the nested lists
  private def parseLine(input: String): (Option[Node], String) = {
    val result = ListBuffer.empty[Node]
    var remaining = ""
    val numPattern = "^(\\d+)(.*)".r

    input match {
      case s"[$rest" =>
        remaining = rest
        do {
          val (next, remainingNext) = parseLine(remaining)
          remaining = remainingNext
          next match {
            case Some(value) => result.append(value)
            case None => return (Some(ListNode(result)), remaining)
          }
        }
        while (remaining.nonEmpty)
      case s",$rest" => return parseLine(rest)
      case s"]$rest" => return if (result.nonEmpty) (Some(ListNode(result)), rest) else (None, rest)
      case numPattern(x, rest) => return (Some(ValueNode(x.toInt)), rest)
    }
    (Some(ListNode(result)), remaining)
  }

  private def compare(a: Node, b: Node): Int = {
    (a, b) match {
      // both are values
      case (ValueNode(left), ValueNode(right)) => left - right
      // both are empty
      case (ListNode(left), ListNode(right)) if left.isEmpty && right.isEmpty => 0
      // left is empty
      case (ListNode(left), _) if left.isEmpty => -1
      // right is empty
      case (_, ListNode(right)) if right.isEmpty => 1
      // both lists, compare first and potentially do recursion
      case (ListNode(left), ListNode(right)) =>
        val firstResult = compare(left.head, right.head)
        if (firstResult == 0) {
          compare(ListNode(left.tail), ListNode(right.tail))
        } else {
          firstResult
        }
      // left is a value, right is a list -> convert left to list
      case (ValueNode(left), right) => compare(ListNode(ListBuffer(ValueNode(left))), right)
      // left is a list, right is a value -> convert right to list
      case (left, ValueNode(right)) => compare(left, ListNode(ListBuffer(ValueNode(right))))
    }
  }

  def run1(input: List[String]): Int = {
    val pairs = splitAt(input, "").map(pair => {
      pair.map(line => parseLine(line)._1.get)
    })

    pairs.zipWithIndex.map { case (List(first, second), i) =>
      if (compare(first, second) <= 0) {
        i + 1
      } else {
        0
      }
    }.sum
  }

  def run2(input: List[String]): Int = {
    val dividerA = "[[2]]"
    val dividerB = "[[6]]"
    val sorted = input.appended(dividerA).appended(dividerB).filterNot(_ == "").map(parseLine(_)._1.get).sortWith((a, b) => compare(a, b) < 0)
    (sorted.indexWhere(_.toString == dividerA) + 1) * (sorted.indexWhere(_.toString == dividerB) + 1)
  }

  println("[Part1]:", run1(readFileByLine("day13-input.txt")))
  println("[Part2]:", run2(readFileByLine("day13-input.txt")))
}

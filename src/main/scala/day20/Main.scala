package day20

import utils.Utils._

object Main extends App {
  case class Node(value: Int, var next: Node, var previous: Node) {
    override def toString: String = s"(${previous.value}) <- ($value) -> (${next.value})"
  }

  def run1(input: List[String]): Int = {
    val nodes = input.map(n => Node(value = n.toInt, next = null, previous = null))

    nodes.sliding(2).foreach({ case List(a, b) =>
      a.next = b
      b.previous = a
    })
    nodes.head.previous = nodes.last
    nodes.last.next = nodes.head

    for {
      node <- nodes
      _ <- 0 until node.value.abs
    } {
      if (node.value > 0) {
        node.previous.next = node.next
        node.next.previous = node.previous
        node.previous = node.next
        node.next = node.next.next
        node.previous.next = node
        node.next.previous = node
      } else {
        node.next.previous = node.previous
        node.previous.next = node.next
        node.previous = node.previous.previous
        node.next = node.next.previous
        node.previous.next = node
        node.next.previous = node
      }
    }

    var currentNode = nodes.find(_.value == 0).get
    var sum = 0

    for (i <- 1 to 3000) {
      currentNode = currentNode.next
      if (i % 1000 == 0) {
        sum += currentNode.value
      }
    }

    sum
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day20-input.txt")))
  //  println("[Part2]:", run2(readFileByLine("day20-test01.txt")))
}

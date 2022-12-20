package day20

import utils.Utils._

object Main extends App {
  case class Node(value: Long, var next: Node, var previous: Node) {
    override def toString: String = s"(${previous.value}) <- ($value) -> (${next.value})"
  }

  private def parse(input: List[String], multiplier: Long = 1L): List[Node] = {
    val nodes = input.map(n => Node(value = n.toLong * multiplier, next = null, previous = null))

    nodes.sliding(2).foreach({ case List(a, b) =>
      a.next = b
      b.previous = a
    })
    nodes.head.previous = nodes.last
    nodes.last.next = nodes.head

    nodes
  }

  private def mix(nodes: List[Node], iterations: Int): Unit = {
    for {
      _ <- 1 to iterations
      node <- nodes
      _ <- 0L until node.value.abs % (nodes.length - 1)
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
  }

  private def findResult(nodes: List[Node]): Long = {
    var currentNode = nodes.find(_.value == 0).get
    var sum = 0L

    for (i <- 1 to 3000) {
      currentNode = currentNode.next
      if (i % 1000 == 0) {
        sum += currentNode.value
      }
    }

    sum
  }

  def run1(input: List[String]): Long = {
    val nodes = parse(input)
    mix(nodes, 1)
    findResult(nodes)
  }

  def run2(input: List[String]): Long = {
    val nodes = parse(input, multiplier = 811589153L)
    mix(nodes, 10)
    findResult(nodes)
  }

  println("[Part1]:", run1(readFileByLine("day20-input.txt")))
  println("[Part2]:", run2(readFileByLine("day20-input.txt")))
}

package day05

import utils.Utils._

object Main extends App {
  case class Instruction(move: Int, from: Int, to: Int)

  case class Stack(name: String, items: List[String])

  def parseStacks(inputLines: List[String]): List[Stack] = {
    val lines = inputLines.reverse.map(_.grouped(4).toList).map(_.map(str => str.replace("[", "").replace("]", "").trim))
    val stackNames = lines.last
    val stackCount = stackNames.length
    val zeroFilled = lines.dropRight(1).map(list => list ++ List.fill(stackCount - list.length)(""))

    (0 until stackCount).map(i => {
      val stackName = stackNames(i)
      val stackItems = zeroFilled.map(line => line(i)).filterNot(_ == "")
      Stack(stackName, stackItems)
    }).toList
  }

  def parseInstructions(lines: List[String]): List[Instruction] = {
    lines.map(line => {
      val first :: rest :: _ = line.split(" from ").toList
      val fromTo = rest.split(" to ").map(_.toInt).toList
      Instruction(first.replace("move ", "").toInt, fromTo.head, fromTo.last)
    })
  }

  def run1(input: List[String]): String = {
    val parts = splitAt(input, "")
    val stacks = parseStacks(parts.head)

    val instructions = parseInstructions(parts.last)

    val resultState = instructions.foldRight(stacks)((instruction, state) => {
      val stackFrom = state.find(_.name == instruction.from.toString).get
      val toMove = stackFrom.items.take(instruction.move)
      val remaining = stackFrom.items.drop(instruction.move)
      val modifiedFrom = stackFrom.copy(items = remaining)

      val stackTo = state.find(_.name == instruction.to.toString).get

      state.map(stack => {
        stack.name match {
          case stackFrom.name => modifiedFrom
          case stackTo.name => stack.copy(items = toMove.reverse ++ stack.items)
          case _ => stack
        }
      })
    })

    resultState.map(_.items.head).mkString
  }

  def run2(input: List[String]): String = {
    val parts = splitAt(input, "")
    val stacks = parseStacks(parts.head)

    val instructions = parseInstructions(parts.last)

    val resultState = instructions.foldRight(stacks)((instruction, state) => {
      val stackFrom = state.find(_.name == instruction.from.toString).get
      val toMove = stackFrom.items.take(instruction.move)
      val remaining = stackFrom.items.drop(instruction.move)
      val modifiedFrom = stackFrom.copy(items = remaining)

      val stackTo = state.find(_.name == instruction.to.toString).get

      state.map(stack => {
        stack.name match {
          case stackFrom.name => modifiedFrom
          case stackTo.name => stack.copy(items = toMove ++ stack.items)
          case _ => stack
        }
      })
    })

    resultState.map(_.items.head).mkString
  }

  println("[Part1]:", run1(readFileByLine("day05-input.txt")))
  println("[Part2]:", run2(readFileByLine("day05-input.txt")))
}

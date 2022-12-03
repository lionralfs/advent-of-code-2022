#!/bin/bash

# usage: ./mkday.sh 04

DAY=$1

touch src/main/resources/day$DAY-input.txt
touch src/main/resources/day$DAY-test01.txt

mkdir -p src/main/scala/day$DAY

cat <<EOM >src/main/scala/day$DAY/Main.scala
package day$DAY

import utils.Utils._

object Main extends App {
  def run1(input: List[String]): Int = {
    1
  }

  def run2(input: List[String]): Int = {
    1
  }

  println("[Part1]:", run1(readFileByLine("day$DAY-input.txt")))
  println("[Part2]:", run2(readFileByLine("day$DAY-input.txt")))
}
EOM

mkdir -p src/test/scala/day$DAY

cat <<EOM >src/test/scala/day$DAY/Day${DAY}Spec.scala
package day$DAY

import day$DAY.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day${DAY}Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day$DAY-test01.txt")) shouldEqual ???
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day$DAY-input.txt")) shouldEqual ???
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day$DAY-test01.txt")) shouldEqual ???
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day$DAY-input.txt")) shouldEqual ???
  }
}
EOM
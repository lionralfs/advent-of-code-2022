package day04

import day04.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day04Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day04-test01.txt")) shouldEqual 2
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day04-input.txt")) shouldEqual 507
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day04-test01.txt")) shouldEqual 4
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day04-input.txt")) shouldEqual 897
  }
}

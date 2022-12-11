package day11

import day11.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day11Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day11-test01.txt")) shouldEqual 10605L
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day11-input.txt")) shouldEqual 121450L
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day11-test01.txt")) shouldEqual 2713310158L
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day11-input.txt")) shouldEqual 28244037010L
  }
}

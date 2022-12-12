package day12

import day12.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day12Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day12-test01.txt")) shouldEqual 31
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day12-input.txt")) shouldEqual 534
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day12-test01.txt")) shouldEqual 29
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day12-input.txt")) shouldEqual 525
  }
}

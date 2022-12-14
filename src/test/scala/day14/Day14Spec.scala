package day14

import day14.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day14Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day14-test01.txt")) shouldEqual ???
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day14-input.txt")) shouldEqual ???
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day14-test01.txt")) shouldEqual ???
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day14-input.txt")) shouldEqual ???
  }
}

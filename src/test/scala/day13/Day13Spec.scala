package day13

import day13.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day13Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day13-test01.txt")) shouldEqual 13
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day13-input.txt")) shouldEqual 5938
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day13-test01.txt")) shouldEqual 140
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day13-input.txt")) shouldEqual 29025
  }
}

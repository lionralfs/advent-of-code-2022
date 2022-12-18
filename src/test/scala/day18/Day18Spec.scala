package day18

import day18.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day18Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day18-test01.txt")) shouldEqual 64
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day18-input.txt")) shouldEqual 3500
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day18-test01.txt")) shouldEqual 58
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day18-input.txt")) shouldEqual 2048
  }
}

package day03

import day03.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day03Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day03-test01.txt")) shouldEqual 157
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day03-input.txt")) shouldEqual 8123
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day03-test01.txt")) shouldEqual 70
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day03-input.txt")) shouldEqual 2620
  }
}

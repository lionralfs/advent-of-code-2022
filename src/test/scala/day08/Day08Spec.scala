package day08

import day08.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day08Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day08-test01.txt")) shouldEqual 21
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day08-input.txt")) shouldEqual 1662
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day08-test01.txt")) shouldEqual 8
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day08-input.txt")) shouldEqual 537600
  }
}

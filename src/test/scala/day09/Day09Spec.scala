package day09

import day09.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day09Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day09-test01.txt")) shouldEqual 13
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day09-input.txt")) shouldEqual 6098
  }

  "Part 2" should "calculate the first test input" in {
    run2(Utils.readFileByLine("day09-test01.txt")) shouldEqual 1
  }

  it should "calculate the second test input" in {
    run2(Utils.readFileByLine("day09-test02.txt")) shouldEqual 36
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day09-input.txt")) shouldEqual 2597
  }
}

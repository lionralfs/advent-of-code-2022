package day01

import day01.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day01Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day01-test01.txt")) shouldEqual 24000
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day01-input.txt")) shouldEqual 67027
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day01-test01.txt")) shouldEqual 45000
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day01-input.txt")) shouldEqual 197291
  }
}

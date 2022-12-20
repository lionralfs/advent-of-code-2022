package day20

import day20.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day20Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day20-test01.txt")) shouldEqual 3
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day20-input.txt")) shouldEqual 11073
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day20-test01.txt")) shouldEqual 0
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day20-input.txt")) shouldEqual 0
  }
}

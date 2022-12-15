package day15

import day15.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day15Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day15-test01.txt"), targetRow = 10) shouldEqual 26
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day15-input.txt"), targetRow = 2000000) shouldEqual 5176944
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day15-test01.txt"), 20) shouldEqual 56000011
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day15-input.txt"), 4000000) shouldEqual 13350458933732L
  }
}

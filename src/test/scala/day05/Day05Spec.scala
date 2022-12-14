package day05

import day05.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day05Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day05-test01.txt")) shouldEqual "CMZ"
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day05-input.txt")) shouldEqual "HNSNMTLHQ"
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day05-test01.txt")) shouldEqual "MCD"
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day05-input.txt")) shouldEqual "RNLFDJMCT"
  }
}

package day02

import day02.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day02Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day02-test01.txt")) shouldEqual 15
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day02-input.txt")) shouldEqual 13221
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day02-test01.txt")) shouldEqual 12
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day02-input.txt")) shouldEqual 13131
  }
}

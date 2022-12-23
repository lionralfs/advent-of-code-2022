package day23

import day23.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day23Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day23-test01.txt")) shouldEqual 110
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day23-input.txt")) shouldEqual 4172
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day23-test01.txt")) shouldEqual 20
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day23-input.txt")) shouldEqual 942
  }
}

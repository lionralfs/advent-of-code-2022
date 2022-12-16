package day16

import day16.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day16Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day16-test01.txt")) shouldEqual 1651
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day16-input.txt")) shouldEqual 1915
  }

  //  "Part 2" should "calculate the test input" in {
  //    run2(Utils.readFileByLine("day16-test01.txt")) shouldEqual ???
  //  }

  "Part 2" should "calculate the real input" in {
    run2(Utils.readFileByLine("day16-input.txt")) shouldEqual 2772
  }
}

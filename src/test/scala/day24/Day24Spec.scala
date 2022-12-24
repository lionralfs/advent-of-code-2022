package day24

import day24.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day24Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day24-test02.txt")) shouldEqual 18
  }

//  it should "calculate the real input" in {
  //    run1(Utils.readFileByLine("day24-input.txt")) shouldEqual ???
  //  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day24-test02.txt")) shouldEqual 54
  }

//  it should "calculate the real input" in {
//    run2(Utils.readFileByLine("day24-input.txt")) shouldEqual ???
//  }
}

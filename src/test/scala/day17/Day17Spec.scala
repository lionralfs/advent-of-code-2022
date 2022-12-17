package day17

import day17.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day17Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day17-test01.txt")) shouldEqual 3068
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day17-input.txt")) shouldEqual 3114
  }

//  "Part 2" should "calculate the test input" in {
  //    run2(Utils.readFileByLine("day17-test01.txt")) shouldEqual ???
  //  }
  //
  //  it should "calculate the real input" in {
  //    run2(Utils.readFileByLine("day17-input.txt")) shouldEqual ???
  //  }
}

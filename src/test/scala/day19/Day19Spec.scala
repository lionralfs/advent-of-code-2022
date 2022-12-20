package day19

import day19.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day19Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day19-test01.txt")) shouldEqual 33
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day19-input.txt")) shouldEqual 1766
  }

  //  "Part 2" should "calculate the test input" in {
  //    run2(Utils.readFileByLine("day19-test01.txt")) shouldEqual 3472
  //  }
  //
  //  it should "calculate the real input" in {
  //    run2(Utils.readFileByLine("day19-input.txt")) shouldEqual 30780
  //  }
}

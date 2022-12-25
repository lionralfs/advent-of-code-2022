package day25

import day25.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day25Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day25-test01.txt")) shouldEqual "2=-1=0"
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day25-input.txt")) shouldEqual "2=20---01==222=0=0-2"
  }
}

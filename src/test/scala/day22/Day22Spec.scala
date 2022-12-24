package day22

import day22.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day22Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day22-test01.txt")) shouldEqual 6032
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day22-input.txt")) shouldEqual 56372
  }

//  "Part 2" should "calculate the test input" in {
//    run2(Utils.readFileByLine("day22-test01.txt")) shouldEqual ???
//  }

  "Part 2" should "calculate the real input" in {
    run2(Utils.readFileByLine("day22-input.txt")) shouldEqual 197047
  }
}

package day07

import day07.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day07Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day07-test01.txt")) shouldEqual 95437
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day07-input.txt")) shouldEqual 1118405
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day07-test01.txt")) shouldEqual 24933642
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day07-input.txt")) shouldEqual 12545514
  }
}

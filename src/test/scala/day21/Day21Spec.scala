package day21

import day21.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day21Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day21-test01.txt")) shouldEqual 152
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day21-input.txt")) shouldEqual 43699799094202L
  }

  "Part 2" should "calculate the first test input" in {
    run2(Utils.readFileByLine("day21-test01.txt")) shouldEqual 301
  }

  it should "calculate the second test input" in {
    run2(Utils.readFileByLine("day21-test02.txt")) shouldEqual 19
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day21-input.txt")) shouldEqual 3375719472770L
  }
}

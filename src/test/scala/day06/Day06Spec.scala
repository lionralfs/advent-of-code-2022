package day06

import day06.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day06Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the 1st test input" in {
    run1(Utils.readFileByLine("day06-test01.txt")) shouldEqual 7
  }

  it should "calculate the 2nd test input" in {
    run1(Utils.readFileByLine("day06-test02.txt")) shouldEqual 5
  }

  it should "calculate the 3rd test input" in {
    run1(Utils.readFileByLine("day06-test03.txt")) shouldEqual 6
  }

  it should "calculate the 4th test input" in {
    run1(Utils.readFileByLine("day06-test04.txt")) shouldEqual 10
  }

  it should "calculate the 5th test input" in {
    run1(Utils.readFileByLine("day06-test05.txt")) shouldEqual 11
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day06-input.txt")) shouldEqual 1896
  }

  "Part 2" should "calculate the 1st test input" in {
    run2(Utils.readFileByLine("day06-test01.txt")) shouldEqual 19
  }

  it should "calculate the 2nd test input" in {
    run2(Utils.readFileByLine("day06-test02.txt")) shouldEqual 23
  }

  it should "calculate the 3rd test input" in {
    run2(Utils.readFileByLine("day06-test03.txt")) shouldEqual 23
  }

  it should "calculate the 4th test input" in {
    run2(Utils.readFileByLine("day06-test04.txt")) shouldEqual 29
  }

  it should "calculate the 5th test input" in {
    run2(Utils.readFileByLine("day06-test05.txt")) shouldEqual 26
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day06-input.txt")) shouldEqual 3452
  }
}

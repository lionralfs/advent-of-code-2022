package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilsSpec extends AnyFlatSpec with Matchers {

  "splitAt" should "preserve order" in {
    Utils.splitAt(List(1, 2, 3, 4, 5), 3) shouldEqual List(List(1, 2), List(4, 5))
  }

  it should "work for other data types" in {
    Utils.splitAt(List("1", "2", "3"), "2") shouldEqual List(List("1"), List("3"))
  }

  "readFileByLine" should "preserve order" in {
    Utils.readFileByLine("day01-test01.txt") shouldEqual List(
      "1000",
      "2000",
      "3000",
      "",
      "4000",
      "",
      "5000",
      "6000",
      "",
      "7000",
      "8000",
      "9000",
      "",
      "10000"
    )
  }
}

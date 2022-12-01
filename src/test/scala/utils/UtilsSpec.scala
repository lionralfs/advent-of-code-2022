package utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilsSpec extends AnyFlatSpec with Matchers {

  "splitAt" should "preserve order" in {
    Utils.splitAt(List(1, 2, 3), 2) shouldEqual List(List(1), List(3))
  }

  it should "work for other data types" in {
    Utils.splitAt(List("1", "2", "3"), "2") shouldEqual List(List("1"), List("3"))
  }
}

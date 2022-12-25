package day25

import utils.Utils._

object Main extends App {
  private def snafuToDecimal(snafu: String): Long = {
    snafu.toList.reverse.zipWithIndex.foldLeft(0L)({
      case (acc, (char, i)) =>
        val value = char match {
          case '2' => 2
          case '1' => 1
          case '0' => 0
          case '-' => -1
          case '=' => -2
        }
        acc + value * math.pow(5, i).toLong
    })
  }


  private def decimalToSnafu(decimal: Long): String = {
    var rest = decimal
    var result: List[Char] = Nil

    while (rest > 0) {
      result = "012=-".charAt((rest % 5).toInt) :: result
      rest -= (((rest + 2) % 5) - 2)
      rest /= 5
    }
    result.mkString("")
  }

  def run1(input: List[String]): String = {
    val sumDecimal = input.map(snafuToDecimal).sum
    decimalToSnafu(sumDecimal)
  }

  println("[Part1]:", run1(readFileByLine("day25-input.txt")))
}

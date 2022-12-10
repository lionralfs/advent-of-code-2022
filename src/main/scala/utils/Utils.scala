package utils

import scala.io.Source

object Utils {
  private val RESOURCES_DIR = "src/main/resources"

  def readFileByLine(inputPath: String): List[String] = {
    val source = Source.fromFile(s"$RESOURCES_DIR/$inputPath")
    source.getLines.toList
  }

  def readFileByLineAsInt(inputPath: String): List[Int] = {
    readFileByLine(inputPath).map(_.toInt)
  }

  def splitAt[ElementType](list: List[ElementType], separator: ElementType): List[List[ElementType]] = {
    list.foldRight[List[List[ElementType]]](Nil)((curr, acc) => {
      if (curr == separator) {
        List() :: acc
      } else {
        acc match {
          case head :: tail => (curr :: head) :: tail
          case _ => List(List(curr))
        }
      }
    })
  }

  // taken from https://oceanpad.github.io/2016/09/27/2016/09/scala-Measure-and-display-the-method-running-time/
  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("========================================")
    println("Elapsed time: " + (t1 - t0) + "ms")
    println("========================================")
    result
  }
}

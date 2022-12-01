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

  def splitAt(list: List[String], char: String): List[List[String]] = {
    list.foldLeft[List[List[String]]](List())((acc, curr) => {
      if (curr == char) {
        List() :: acc
      } else {
        acc match {
          case head :: tail => head.appended(curr) :: tail
          case _ => List(List(curr))
        }
      }
    })
  }
}

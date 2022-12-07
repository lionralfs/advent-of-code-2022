package day07

import utils.Utils._

object Main extends App {
  case class File(name: String, size: Int)

  case class Directory(var parent: Option[Directory], var directories: List[Directory], var files: List[File], name: String)

  def run1(input: List[String]): Int = {
    val cdPattern = "\\$ cd (.+)".r
    val dirOutputPattern = "dir (.+)".r
    val fileOutputPattern = "(\\d+) (.+)".r

    var currentDir: Directory = null

    input.foreach {
      case cdPattern(dir) =>
        if (dir == "/") {
          currentDir = Directory(parent = None, directories = Nil, files = Nil, name = "/")
        } else if (dir == "..") {
          currentDir = currentDir.parent.get
        } else {
          currentDir = currentDir.directories.find(_.name == dir).get
        }
      case "$ ls" => ""
      case dirOutputPattern(dir) =>
        if (!currentDir.directories.exists(_.name == dir)) {
          val newDir = Directory(parent = Some(currentDir), directories = Nil, files = Nil, name = dir)
          currentDir.directories = currentDir.directories.appended(newDir)
        }
      case fileOutputPattern(size, name) =>
        val newFile = File(name = name, size = size.toInt)
        currentDir.files = currentDir.files.appended(newFile)
    }

    // walk to root
    while (currentDir.parent.isDefined) currentDir = currentDir.parent.get

    var sumOfDirSizes = 0

    def dirSize(dir: Directory): Int = {
      val fileSizes = dir.files.map(_.size).sum
      val total = dir.directories.map(dirSize).sum + fileSizes
      if (total <= 100000) {
        sumOfDirSizes += total
      }
      total
    }

    dirSize(currentDir)

    sumOfDirSizes
  }


  def run2(input: List[String]): Int = {
    val cdPattern = "\\$ cd (.+)".r
    val dirOutputPattern = "dir (.+)".r
    val fileOutputPattern = "(\\d+) (.+)".r

    var currentDir: Directory = null

    input.foreach {
      case cdPattern(dir) =>
        if (dir == "/") {
          currentDir = Directory(parent = None, directories = Nil, files = Nil, name = "/")
        } else if (dir == "..") {
          currentDir = currentDir.parent.get
        } else {
          currentDir = currentDir.directories.find(_.name == dir).get
        }
      case "$ ls" => ""
      case dirOutputPattern(dir) =>
        if (!currentDir.directories.exists(_.name == dir)) {
          val newDir = Directory(parent = Some(currentDir), directories = Nil, files = Nil, name = dir)
          currentDir.directories = currentDir.directories.appended(newDir)
        }
      case fileOutputPattern(size, name) =>
        val newFile = File(name = name, size = size.toInt)
        currentDir.files = currentDir.files.appended(newFile)
    }

    // walk to root
    while (currentDir.parent.isDefined) currentDir = currentDir.parent.get

    var dirSizes: Map[String, Int] = Map.empty

    def dirSize(dir: Directory): Int = {
      val fileSizes = dir.files.map(_.size).sum
      val total = dir.directories.map(dirSize).sum + fileSizes
      dirSizes = dirSizes.updated(dir.name, total)
      total
    }

    val freeSpace = 70000000 - dirSize(currentDir)
    dirSizes.values.toList.sorted.find(freeSpace + _ >= 30000000).get
  }

  println("[Part1]:", run1(readFileByLine("day07-input.txt")))
  println("[Part2]:", run2(readFileByLine("day07-input.txt")))
}

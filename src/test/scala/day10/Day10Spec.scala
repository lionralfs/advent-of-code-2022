package day10

import day10.Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Utils

class Day10Spec extends AnyFlatSpec with Matchers {

  "Part 1" should "calculate the test input" in {
    run1(Utils.readFileByLine("day10-test02.txt")) shouldEqual 13140
  }

  it should "calculate the real input" in {
    run1(Utils.readFileByLine("day10-input.txt")) shouldEqual 14520
  }

  "Part 2" should "calculate the test input" in {
    run2(Utils.readFileByLine("day10-test02.txt")) shouldEqual "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######....."
  }

  it should "calculate the real input" in {
    run2(Utils.readFileByLine("day10-input.txt")) shouldEqual "###..####.###...##..####.####...##.###..\n#..#....#.#..#.#..#....#.#.......#.#..#.\n#..#...#..###..#......#..###.....#.###..\n###...#...#..#.#.##..#...#.......#.#..#.\n#....#....#..#.#..#.#....#....#..#.#..#.\n#....####.###...###.####.####..##..###.."
  }
}

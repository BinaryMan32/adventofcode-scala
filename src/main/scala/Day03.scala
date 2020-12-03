package com.fivebytestudios.wildfreddy

object Day03 {
  private def countTrees(input: List[String], slope: (Int, Int)): Long = {
    val (dx, dy) = slope
    (0 until input.length by dy)
      .map(input)
      .zip(0 to Int.MaxValue by dx)
      .map{case (row, x) => row(x % row.length)}
      .count(_ == '#')
  }

  def part1(input: List[String]): Long =
    countTrees(input, (3, 1))

  def part2(input: List[String]): Long =
    Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(countTrees(input, _))
      .product
}

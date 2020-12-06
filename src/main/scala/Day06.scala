package com.fivebytestudios.wildfreddy

object Day06 {
  def part1(input: List[String]): Long = {
    Utilities.groupLines(input)
      .map(_.map(_.toSet).reduce(_ ++ _))
      .map(_.size)
      .sum
  }

  def part2(input: List[String]): Long = {
    Utilities.groupLines(input)
      .map(_.map(_.toSet).reduce(_ intersect _))
      .map(_.size)
      .sum
  }
}

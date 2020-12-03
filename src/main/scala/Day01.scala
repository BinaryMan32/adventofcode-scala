package com.fivebytestudios.wildfreddy

object Day01 {
  def part1(input: List[Int]): Option[Int] = {
    input
      .combinations(2)
      .find(_.sum == 2020)
      .map(_.product)
  }

  def part2(input: List[Int]): Option[Int] = {
    input
      .combinations(3)
      .find(_.sum == 2020)
      .map(_.product)
  }
}

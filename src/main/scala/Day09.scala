package com.fivebytestudios.wildfreddy

object Day09 {
  def findInvalid(input: List[Long], preambleLength: Int): Option[Long] =
    input
      .sliding(preambleLength + 1)
      .find { numbers =>
        !numbers
          .take(preambleLength)
          .combinations(2)
          .exists(_.sum == numbers.last)
      }
      .map(_.last)

  def part1(input: List[String], preambleLength: Int): Option[Long] = {
    findInvalid(input.map(_.toLong), preambleLength)
  }

  def part2(input: List[String], preambleLength: Int): Option[Long] = {
    val numbers = input.map(_.toLong)
    findInvalid(numbers, preambleLength).flatMap(invalid =>
      (2 until numbers.length)
        .flatMap(numbers.sliding)
        .find(_.sum == invalid)
        .map(contiguous => contiguous.min + contiguous.max)
    )
  }
}

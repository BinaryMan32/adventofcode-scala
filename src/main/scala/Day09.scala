package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

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

  def takeUntilLimit(input: List[Long], limit: Long): List[Long] = {
    @tailrec
    def takeUntilLimitAcc(prefix: List[Long], remainder: List[Long], sum: Long): List[Long] = {
      val newSum = remainder.head + sum
      if (newSum <= limit)
        takeUntilLimitAcc(remainder.head :: prefix, remainder.tail, newSum)
      else
        prefix
    }
    takeUntilLimitAcc(List.empty, input, 0).reverse
  }

  def findPrefixWithSum(input: List[Long], sum: Long): Option[List[Long]] = {
    Some(takeUntilLimit(input, sum))
      .filter(prefix => prefix.sum == sum && prefix.size >= 2)
  }

  def part2(input: List[String], preambleLength: Int): Option[Long] = {
    val numbers = input.map(_.toLong)
    findInvalid(numbers, preambleLength).flatMap(invalid =>
      numbers
        .tails
        .map(findPrefixWithSum(_, invalid))
        .collectFirst{ case Some(contiguous) => contiguous.min + contiguous.max }
    )
  }
}

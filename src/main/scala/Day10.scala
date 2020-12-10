package com.fivebytestudios.wildfreddy

object Day10 {
  def deltas(input: List[String]): List[Int] = {
    val sorted = input.map(_.toInt).sorted
    (sorted.appended(sorted.last+3) zip sorted.prepended(0))
      .map{case (a, b) => a - b}
  }

  def part1(input: List[String]): Map[Int, Int] = {
    deltas(input)
      .groupMapReduce(identity)(_ => 1)(_ + _)
  }

  def findRuns(input: List[Int]): List[List[Int]] = {
    val (_, remainder1) = input.span(_ == 3)
    val (keep, remainder2) = remainder1.span(_ != 3)
    keep :: (if (remainder2.nonEmpty) findRuns(remainder2) else Nil)
  }

  // this seems to be the only possibilities that occur in the input
  // would be nice to find the pattern
  val combos = Map(
    2 -> 2,
    3 -> 4,
    4 -> 7
  )

  def part2(input: List[String]): Long = {
    val runs = findRuns(deltas(input))
      .filter(_.size >= 2)
    // relies on all adapters bridging 1 or 3 jolts, there are no 2's
    runs
      .groupMapReduce(_.size)(_ => 1)(_ + _)
      .map{case (ones, count) => Math.pow(combos(ones), count).toLong}
      .product
  }
}

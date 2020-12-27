package com.fivebytestudios.wildfreddy

object Day25 {
  case class Transformer(subject: Long) {
    def once(value: Long): Long =
      (value * subject) % 20201227
    def iterate: Iterator[Long] =
      Iterator.iterate(1L)(once)
    def many(count: Int): Long =
      iterate.drop(count).next
  }

  // Returns the key found and number of loops to compute it
  def findKeyLoopSize(keys: List[Long]): (Long, Int) =
    Transformer(subject = 7)
      .iterate
      .zipWithIndex
      .find { case (maybeKey, _) => keys.contains(maybeKey) }
      .get

  def part1(input: List[String]): Long = {
    val keys = input.map(_.toLong)
    val (key, loopSize) = findKeyLoopSize(keys)
    val otherKey = keys(keys.indexOf(key) ^ 1)
    Transformer(subject = otherKey).many(loopSize)
  }

  def part2(input: List[String]): Long = {
    0
  }
}

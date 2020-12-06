package com.fivebytestudios.wildfreddy

object Day05 {
  val mapping = Map('F' -> '0', 'B' -> '1', 'L' -> '0', 'R' -> '1')
  def seats(input: List[String]): List[Int] =
    input
      .map(line => Integer.parseInt(line.map(mapping), 2))

  def part1(input: List[String]): Long =
    seats(input).max

  def part2(input: List[String]): Long = {
    val occupied = seats(input)
    val neighbors = occupied.map(_ - 1).toSet intersect occupied.map(_ + 1).toSet
    val possibilities = neighbors -- occupied.toSet
    possibilities.head
  }
}

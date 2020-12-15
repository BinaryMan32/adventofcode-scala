package com.fivebytestudios.wildfreddy

object Day13 {
  def part1(input: List[String]): Long = {
    val startTimestamp = input.head.toInt
    val buses = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val (id, wait) = buses
      .map(id => id -> (id - (startTimestamp % id)) % id)
      .minBy(_._2)
    id * wait
  }

  def part2(input: List[String]): Long = {
    0
  }
}

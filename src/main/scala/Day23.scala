package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

object Day23 {
  /**
   * Not idiomatic scala, but I didn't see a way to do 10,000,000 turns efficiently without
   * modifying state. Any approach I can think of ends up essentially copying the state of
   * all cups on each iteration. This runs quickly, and is relatively easy to follow.
   */
  class CrabCups(nextCup: Array[Int], var currentCup: Int, minCup: Int, maxCup: Int) {
    def move(): Unit = {
      val picked = after(currentCup, 3).toVector
      val destination = pickDestinationCup(currentCup - 1, picked)
      nextCup(currentCup) = nextCup(picked.last)
      nextCup(picked.last) = nextCup(destination)
      nextCup(destination) = picked.head
      currentCup = nextCup(currentCup)
    }
    def after(cup: Int, count: Int): Iterator[Int] =
      Iterator.iterate(nextCup(cup))(nextCup).take(count)
    @tailrec
    private def pickDestinationCup(candidate: Int, picked: Vector[Int]): Int = {
      if (candidate < minCup)
        pickDestinationCup(maxCup, picked)
      else if (picked.contains(candidate))
        pickDestinationCup(candidate - 1, picked)
      else
        candidate
    }
  }

  object CrabCups {
    def apply(cups: IndexedSeq[Int]): CrabCups = {
      val maxCup = cups.max
      val cupsArray = Array.fill(maxCup + 1)(0)
      for ((cup, next) <- cups zip cups.drop(1))
        cupsArray(cup) = next
      cupsArray(cups.last) = cups.head
      new CrabCups(nextCup = cupsArray, currentCup = cups.head,
        minCup = cups.min, maxCup = cups.max)
    }
  }

  def part1(input: String): String = {
    val crabCups = CrabCups(input.map(_.toString.toInt))
    for (_ <- 0 until 100) crabCups.move()
    crabCups.after(1, input.length - 1).map(_.toString).mkString
  }

  def part2(input: String): Long = {
    val startingCups = input.map(_.toString.toInt).toVector
    val crabCups = CrabCups(startingCups ++ (startingCups.max + 1 to 1000000))
    for (_ <- 0 until 10000000) crabCups.move()
    crabCups.after(1, 2).map(_.toLong).product
  }
}

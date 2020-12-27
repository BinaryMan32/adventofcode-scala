package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

object Day13 {
  def part1(input: List[String]): Long = {
    val startTimestamp = input.head.toInt
    val buses = input(1).split(",").filterNot(_ == "x").map(_.toInt)
    val (id, wait) = buses
      .map(id => id -> (id - (startTimestamp % id)) % id)
      .minBy(_._2)
    id * wait
  }

  case class Constraint(offset: Long, period: Long) {
    def possibilities: Iterator[Long] =
      Iterator.iterate(offset)(_ + period)
    def satisfies(test: Long): Boolean =
      (test - offset) % period == 0
  }

  def reduceConstraints(a: Constraint, b: Constraint): Constraint =
    if (a.period > b.period)
      reduceConstraints(b, a)
    else
      Constraint(
        offset = b.possibilities.find(a.satisfies).get,
        period = a.period * b.period
      )

  def parseConstraints(line: String): Iterable[Constraint] =
    line
      .split(",")
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map{ case(bus, index) =>
        Constraint(
          offset = bus.toInt - index,
          period = bus.toInt
        )
      }

  def findConstraintTime(line: String): Long =
    parseConstraints(line)
      .reduce(reduceConstraints)
      .offset

  def part2(input: List[String]): Long = {
    findConstraintTime(input(1))
  }
}

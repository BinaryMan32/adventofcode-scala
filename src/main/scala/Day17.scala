package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

object Day17 {
  case class Coordinate(values: Vector[Int]) {
    import Coordinate._
    def neighbors: Set[Coordinate] =
      neighborVectors(values)
        .filterNot(_ == values)
        .map(Coordinate(_))
  }

  object Coordinate {
    @tailrec
    def neighborVectors(
      coordinate: Vector[Int],
      accumulate: Set[Vector[Int]] = Set(Vector.empty)
    ): Set[Vector[Int]] =
      coordinate match {
        case head +: tail => neighborVectors(
          tail,
          accumulate.flatMap(acc =>
            (head - 1 to head + 1).map(acc :+ _)
          )
        )
        case _ => accumulate
      }
  }

  case class State(cubes: Set[Coordinate]) {
    def next: State = {
      val newCubes = cubes
        .toList
        .flatMap(_.neighbors)
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .filter { case (coordinate, neighbors) =>
          neighbors == 3 || neighbors == 2 && cubes(coordinate)
        }.keySet
      State(newCubes)
    }
  }

  def initialState(input: List[String], additionalDimensions: Int) : State = {
    val cubes = for {
      (row, y) <- input.zipWithIndex
      (value, x) <- row.zipWithIndex if value == '#'
    } yield Vector(x, y)
    State(
      cubes.map(c => Coordinate(c ++ Vector.fill(additionalDimensions)(0)))
        .toSet
    )
  }

  def simulate(state: State, cycles: Int): State =
    Iterator
      .iterate(state)(_.next)
      .drop(6)
      .next

  def part1(input: List[String]): Long = {
    simulate(initialState(input, additionalDimensions = 1), cycles=6)
      .cubes
      .size
  }

  def part2(input: List[String]): Long = {
    simulate(initialState(input, additionalDimensions = 2), cycles=6)
      .cubes
      .size
  }
}

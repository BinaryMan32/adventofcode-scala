package com.fivebytestudios.wildfreddy

object SeatState extends Enumeration {
  type SeatState = Value
  val Empty = Value
  val Occupied = Value
  val Floor = Value
}

object Day11 {
  import SeatState._

  case class State(seats: Array[Array[SeatState]]) {
    def numRows: Int = seats.length
    def numCols: Int = seats(0).length

    def rowIndices: Range = seats.indices
    def colIndices: Range = seats(0).indices

    def count(state: SeatState): Int =
      seats.flatten.count(_ == state)

    override def toString: String = {
      s"occupied: ${count(Occupied)}\n" + seats.map(
        _.map(State.seatStateToChar).mkString
      ).mkString("\n")
    }

    def neighborhood(r: Int, c: Int): Map[SeatState, Int] =
      seats
        .slice(Math.max(0, r - 1), Math.min(r + 2, numRows))
        .flatMap(_.slice(Math.max(0, c - 1), Math.min(c + 2, numCols)))
        .groupMapReduce(identity)(_ => 1)(_ + _)

    def nextNeighbors: State = {
      new State(
        (0 until numRows).map(r =>
          (0 until numCols).map { c =>
            val neighborCounts = neighborhood(r, c)
            seats(r)(c) match {
              case Empty if neighborCounts.getOrElse(Occupied, 0) == 0 => Occupied
              case Occupied if neighborCounts(Occupied) >= 5 => Empty
              case state => state
            }
          }.toArray
        ).toArray
      )
    }

    def addTuple(a: (Int, Int), b: (Int, Int)): (Int, Int) =
      (a._1 + b._1, a._2 + b._2)

    val directions: List[(Int, Int)] = (for {
      r <- -1 to 1
      c <- -1 to 1
    } yield (r, c)).filterNot(_ == (0, 0)).toList

    def seatRays(r: Int, c: Int): Map[SeatState, Int] =
      directions.flatMap(delta =>
        Iterator.iterate(addTuple((r, c), delta))(addTuple(_, delta))
          .takeWhile{case (r, c) => rowIndices.contains(r) && colIndices.contains(c)}
          .collectFirst{case (r, c) if seats(r)(c) != Floor => seats(r)(c)}
      ).groupMapReduce(identity)(_ => 1)(_ + _)

    def nextRays: State = {
      new State(
        (0 until numRows).map(r =>
          (0 until numCols).map { c =>
            val rayCounts = seatRays(r, c)
            seats(r)(c) match {
              case Empty if rayCounts.getOrElse(Occupied, 0) == 0 => Occupied
              case Occupied if rayCounts.getOrElse(Occupied, 0) >= 5 => Empty
              case state => state
            }
          }.toArray
        ).toArray
      )
    }
  }

  object State {
    val charToSeatState: Map[Char, SeatState] = Map(
      'L' -> Empty,
      '#' -> Occupied,
      '.' -> Floor
    )
    val seatStateToChar: Map[SeatState, Char] = charToSeatState.map(_.swap)

    def apply(input: List[String]) =
      new State(
        input.map(
          _.map(charToSeatState).toArray
        ).toArray
      )
  }

  def runUntilSettles(states: Iterator[State]): Int = {
    states.map { state =>
      println(state)
      println()
      state
    }
      .map(_.count(Occupied))
      .sliding(2)
      .collectFirst {
        case Seq(a, b) if a == b => a
      }
      .get
  }

  def part1(input: List[String]): Long = {
    runUntilSettles(Iterator.iterate(State(input))(_.nextNeighbors))
  }

  def part2(input: List[String]): Long = {
    runUntilSettles(Iterator.iterate(State(input))(_.nextRays))
  }
}

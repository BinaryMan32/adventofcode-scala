package com.fivebytestudios.wildfreddy

object Day15 {
  case class State(turn: Int = 0, spoken: Option[Int] = None, history: Map[Int, Int] = Map.empty) {
    def speak = history
      .get(spoken.get)
      .map(older => turn - older)
      .getOrElse(0)
    def next(number: Int): State =
      State(
        turn = turn + 1,
        spoken = Some(number),
        history = history ++ spoken.map(_ -> turn)
      )
  }
  def part1(input: List[Int]): Long = {
    val initial = input.foldLeft(State())((state, num) => state.next(num))
    Iterator.iterate(initial)(state => state.next(state.speak))
      .collectFirst { case state: State if state.turn == 2020 => state.spoken.get }
      .get
  }

  def part2(input: List[Int]): Long = {
    val initial = input.foldLeft(State())((state, num) => state.next(num))
    Iterator.iterate(initial)(state => state.next(state.speak))
      .collectFirst { case state: State if state.turn == 30000000 => state.spoken.get }
      .get
  }
}

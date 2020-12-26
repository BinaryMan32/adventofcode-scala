package com.fivebytestudios.wildfreddy

object Day22 {
  def parseDecks(input: List[String]): List[Vector[Int]] = {
    Utilities.groupLines(input).map(_.tail.map(_.toInt).toVector)
  }
  case class State(decks: List[Vector[Int]]) {
    def nextRound: State = {
      State(decks = if (decks(0).head > decks(1).head)
        List(decks(0).tail ++ Vector(decks(0).head, decks(1).head), decks(1).tail)
      else
        List(decks(0).tail, decks(1).tail ++ Vector(decks(1).head, decks(0).head))
      )
    }
    def hasEnded: Boolean = decks.exists(_.isEmpty)
    def score: Long =
      decks.map(deck =>
        deck.reverse.zipWithIndex.map{ case (card, index) =>
          card.toLong * (index + 1)
        }.sum
      ).max
  }
  def part1(input: List[String]): Long = {
    Iterator.iterate(State(parseDecks(input)))(_.nextRound)
      .find(_.hasEnded)
      .map(_.score)
      .getOrElse(0)
  }

  case class PlayerState(deck: Vector[Int]) {
    def top: Int = deck.head
    def canPlayRecursive: Boolean = deck.tail.size >= deck.head
    def recurse: PlayerState = PlayerState(deck = deck.tail.take(deck.head))
    def next(bottom: Vector[Int]): PlayerState = PlayerState(deck = deck.tail ++ bottom)
    def score: Long =
      deck.reverse.zipWithIndex.map{ case (card, index) =>
        card.toLong * (index + 1)
      }.sum
  }
  case class GameOutcome(winIndex: Int, winState: PlayerState)
  case class RecursiveCombat(players: List[PlayerState], history: Set[List[PlayerState]]) {
    def gameWinner: Option[Int] =
      if (history.contains(players))
        Some(0)
      else
        players
          .zipWithIndex
          .collectFirst{ case (player, index) if player.deck.isEmpty => index ^ 1 }
    def gameOutcome: Option[GameOutcome] = gameWinner.map(winner =>
      GameOutcome(winner, players(winner))
    )
    private def roundWinner: Int =
      if (players.forall(_.canPlayRecursive))
        playRecursiveCombat(players.map(_.recurse)).winIndex
      else if (players(0).top > players(1).top)
        0
      else
        1
    def nextRound: RecursiveCombat = {
      val bottomCards: List[Vector[Int]] = if (roundWinner == 0)
        List(Vector(players(0).top, players(1).top), Vector.empty)
      else
        List(Vector.empty, Vector(players(1).top, players(0).top))
      RecursiveCombat(
        players = (players zip bottomCards).map{case (player, bottom) => player.next(bottom)},
        history = history + players
      )
    }
  }

  def playRecursiveCombat(players: List[PlayerState]): GameOutcome = {
    Iterator.iterate(RecursiveCombat(players, Set.empty))(_.nextRound)
      .find(_.gameWinner.isDefined)
      .flatMap(_.gameOutcome)
      .get
  }

  def playRecursiveCombatInput(input: List[String]): GameOutcome =
    playRecursiveCombat(parseDecks(input).map(PlayerState))

  def part2(input: List[String]): Long = {
    playRecursiveCombatInput(input)
      .winState
      .score
  }
}

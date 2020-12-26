package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day22Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day22"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "parse decks" in {
      Day22.parseDecks(exampleData) shouldBe List(
        Vector(9, 2, 6, 3, 1),
        Vector(5, 8, 4, 7, 10)
      )
    }
    "pass example" in {
      Day22.part1(exampleData) shouldBe 306
    }

    "produce result" in {
      Day22.part1(inputData) shouldBe 32489
    }
  }

  "part2" should {
    "player 1 wins due to detecting recursion" in {
      val input = """Player 1:
                    |43
                    |19
                    |
                    |Player 2:
                    |2
                    |29
                    |14
                    |""".stripMargin.split("\n").toList
      Day22.playRecursiveCombatInput(input).winIndex shouldBe 0
    }
    "pass example" in {
      Day22.part2(exampleData) shouldBe 291
    }

    "produce result" in {
      Day22.part2(inputData) shouldBe 35676
    }
  }
}

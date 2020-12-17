package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day16Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day16"
  private val exampleData = getResourceLines("example")
  private val example2Data = getResourceLines("example2")
  private val inputData = getResourceLines("input")

  "parseInput" should {
    "parse example" in {
      import Day16._
      parseInput(exampleData) shouldBe Input(
        fields = FieldDefinitions(
          Map(
            "class" -> FieldValidator(Seq(1 to 3, 5 to 7)),
            "row" -> FieldValidator(Seq(6 to 11, 33 to 44)),
            "seat" -> FieldValidator(Seq(13 to 40, 45 to 50))
          )
        ),
        your = Ticket(Seq(7, 1, 14)),
        nearby = List(
          Ticket(Seq(7, 3, 47)),
          Ticket(Seq(40, 4, 50)),
          Ticket(Seq(55, 2, 20)),
          Ticket(Seq(38, 6, 12))
        )
      )
    }
  }

  "part1" should {
    "pass example" in {
      Day16.part1(exampleData) shouldBe 71
    }

    "produce result" in {
      Day16.part1(inputData) shouldBe 29851
    }
  }

  "part2" should {
    "pass example" in {
    }

    "produce result" in {
      Day16.part2(inputData) shouldBe 3029180675981L
    }
  }
}

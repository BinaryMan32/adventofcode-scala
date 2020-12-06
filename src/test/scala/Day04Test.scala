package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day04Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day04"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day04.part1(exampleData) shouldBe 2
    }

    "produce result" in {
      Day04.part1(inputData) shouldBe 200
    }
  }

  "part2" should {
    "find valid passports" in {
      Day04.part2(getResourceLines("valid")) shouldBe 4
    }

    "find invalid passports" in {
      Day04.part2(getResourceLines("invalid")) shouldBe 0
    }

    "pass example" in {
      Day04.part2(exampleData) shouldBe 2
    }

    "produce result" in {
      Day04.part2(inputData) shouldBe 116
    }
  }
}

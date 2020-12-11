package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day11Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day11"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day11.part1(exampleData) shouldBe 37
    }

    "produce result" in {
      Day11.part1(inputData) shouldBe 2281
    }
  }

  "part2" should {
    "pass example" in {
      Day11.part2(exampleData) shouldBe 26
    }

    "produce result" in {
      Day11.part2(inputData) shouldBe 2085
    }
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day18Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day18"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day18.part1(exampleData) shouldBe (71 + 51)
    }

    "produce result" in {
      Day18.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day18.part2(exampleData) shouldBe (231 + 51)
    }

    "produce result" in {
      Day18.part2(inputData) shouldBe 0
    }
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day06Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day06"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day06.part1(exampleData) shouldBe 11
    }

    "produce result" in {
      Day06.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day06.part2(exampleData) shouldBe 6
    }

    "produce result" in {
      Day06.part2(inputData) shouldBe 0
    }
  }
}

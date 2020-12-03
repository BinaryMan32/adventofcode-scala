package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day03Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day03"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day03.part1(exampleData) shouldBe 7
    }

    "produce result" in {
      Day03.part1(inputData) shouldBe 240
    }
  }

  "part2" should {
    "pass example" in {
      Day03.part2(exampleData) shouldBe 336
    }

    "produce result" in {
      Day03.part2(inputData) shouldBe 2832009600L
    }
  }
}

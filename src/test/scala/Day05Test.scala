package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day05Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day05"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day05.part1(exampleData) shouldBe 820
    }

    "produce result" in {
      Day05.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "produce result" in {
      Day05.part2(inputData) shouldBe 0
    }
  }
}

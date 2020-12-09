package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day09Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day09"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day09.part1(exampleData, preambleLength = 5) shouldBe Some(127)
    }

    "produce result" in {
      Day09.part1(inputData, preambleLength = 25) shouldBe Some(18272118)
    }
  }

  "part2" should {
    "pass example" in {
      Day09.part2(exampleData, preambleLength = 5) shouldBe Some(62)
    }

    "produce result" in {
      Day09.part2(inputData, preambleLength = 25) shouldBe Some(2186361)
    }
  }
}

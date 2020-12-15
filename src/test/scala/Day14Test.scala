package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day14Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day14"
  private val exampleData = getResourceLines("example")
  private val example2Data = getResourceLines("example2")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day14.part1(exampleData) shouldBe 165
    }

    "produce result" in {
      Day14.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day14.part2(example2Data) shouldBe 208
    }

    "produce result" in {
      Day14.part2(inputData) shouldBe 0
    }
  }
}

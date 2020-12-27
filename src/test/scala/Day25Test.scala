package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day25Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day25"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day25.part1(exampleData) shouldBe 14897079
    }

    "produce result" in {
      Day25.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day25.part2(exampleData) shouldBe 0
    }

    "produce result" in {
      Day25.part2(inputData) shouldBe 0
    }
  }
}

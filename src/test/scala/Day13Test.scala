package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day13Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day13"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day13.part1(exampleData) shouldBe 295
    }

    "produce result" in {
      Day13.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day13.part2(exampleData) shouldBe 0
    }

    "produce result" in {
      Day13.part2(inputData) shouldBe 0
    }
  }
}

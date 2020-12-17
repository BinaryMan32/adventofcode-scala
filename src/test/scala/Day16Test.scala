package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day16Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day16"
  private val exampleData = getResourceLines("example")
  private val example2Data = getResourceLines("example2")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day16.part1(exampleData) shouldBe 71
    }

    "produce result" in {
      Day16.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
    }

    "produce result" in {
      Day16.part2(inputData) shouldBe 0
    }
  }
}

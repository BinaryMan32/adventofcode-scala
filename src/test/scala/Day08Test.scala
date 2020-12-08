package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day08Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day08"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day08.part1(exampleData) shouldBe 5
    }

    "produce result" in {
      Day08.part1(inputData) shouldBe 1584
    }
  }

  "part2" should {
    "pass example" in {
      Day08.part2(exampleData) shouldBe 8
    }

    "produce result" in {
      Day08.part2(inputData) shouldBe 920
    }
  }
}

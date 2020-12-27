package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day24Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day24"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day24.part1(exampleData) shouldBe 10
    }

    "produce result" in {
      Day24.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day24.part2(exampleData) shouldBe 2208
    }

    "produce result" in {
      Day24.part2(inputData) shouldBe 0
    }
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day20Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day20"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day20.part1(exampleData) shouldBe 20899048083289L
    }

    "produce result" in {
      Day20.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day20.part2(exampleData) shouldBe 273
    }

    "produce result" in {
      Day20.part2(inputData) shouldBe 0
    }
  }
}

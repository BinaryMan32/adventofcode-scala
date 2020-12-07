package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day07Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day07"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day07.part1(exampleData) shouldBe 4
    }

    "produce result" in {
      Day07.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day07.part2(exampleData) shouldBe 32
    }

    "pass example2" in {
      Day07.part2(getResourceLines("example2")) shouldBe 126
    }

    "produce result" in {
      Day07.part2(inputData) shouldBe 0
    }
  }
}

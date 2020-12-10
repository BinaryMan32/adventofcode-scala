package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day10"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day10.part1(exampleData) should contain theSameElementsAs Map(
        1 -> 7,
        3 -> 5
      )
    }

    "pass example2" in {
      Day10.part1(getResourceLines("example2")) should contain theSameElementsAs Map(
        1 -> 22,
        3 -> 10
      )
    }

    "produce result" in {
      val jolts = Day10.part1(inputData)
      jolts(1) * jolts(3) shouldBe 1836
    }
  }

  "part2" should {
    "pass example" in {
      Day10.part2(exampleData) shouldBe 8
    }

    "pass example2" in {
      Day10.part2(getResourceLines("example2")) shouldBe 19208
    }

    "produce result" in {
      Day10.part2(inputData) shouldBe 43406276662336L
    }
  }
}

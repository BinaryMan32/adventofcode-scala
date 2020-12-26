package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day22Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day22"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day22.part1(exampleData) shouldBe 306
    }

    "produce result" in {
      Day22.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day22.part2(exampleData) shouldBe 291
    }

    "produce result" in {
      Day22.part2(inputData) shouldBe 0
    }
  }
}

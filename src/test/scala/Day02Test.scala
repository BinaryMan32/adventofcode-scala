package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day02Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day02"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day02.part1(exampleData) shouldBe 2
    }

    "produce result" in {
      Day02.part1(inputData) shouldBe 0
    }
  }
}

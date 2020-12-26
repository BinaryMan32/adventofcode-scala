package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day23Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day23"
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day23.part1("389125467") shouldBe "67384529"
    }

    "produce result" in {
      Day23.part1("872495136") shouldBe ""
    }
  }

  "part2" should {
    "pass example" in {
      Day23.part2("389125467") shouldBe 149245887792L
    }

    "produce result" in {
      Day23.part2("872495136") shouldBe 0
    }
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day15Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day15"

  "part1" should {
    "pass example" in {
      Day15.part1(List(0, 3, 6)) shouldBe 436
      Day15.part1(List(1, 3, 2)) shouldBe 1
      Day15.part1(List(2, 1, 3)) shouldBe 10
      Day15.part1(List(1, 2, 3)) shouldBe 27
      Day15.part1(List(2, 3, 1)) shouldBe 78
      Day15.part1(List(3, 2, 1)) shouldBe 438
      Day15.part1(List(3, 1, 2)) shouldBe 1836
    }

    "produce result" in {
      Day15.part1(List(8, 13, 1, 0, 18, 9)) shouldBe 755
    }
  }

  "part2" should {
    "pass example" in {
      Day15.part2(List(0, 3, 6)) shouldBe 175594
      Day15.part2(List(1, 3, 2)) shouldBe 2578
      Day15.part2(List(2, 1, 3)) shouldBe 3544142
      Day15.part2(List(1, 2, 3)) shouldBe 261214
      Day15.part2(List(2, 3, 1)) shouldBe 6895259
      Day15.part2(List(3, 2, 1)) shouldBe 18
      Day15.part2(List(3, 1, 2)) shouldBe 362
    }

    "produce result" in {
      Day15.part2(List(8, 13, 1, 0, 18, 9)) shouldBe 11962
    }
  }
}

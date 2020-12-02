package com.fivebytestudios.wildfreddy

import com.fivebytestudios.wildfreddy.helpers.ResourceHelpers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Day1Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day1"
  private val exampleData = getResourceLines("example").map(_.toInt)
  private val inputData = getResourceLines("input").map(_.toInt)

  "part1" should {
    "pass example" in {
      Day1.part1(exampleData) shouldBe Some(514579)
    }

    "produce result" in {
      Day1.part1(inputData) shouldBe Some(926464)
    }
  }

  "part2" should {
    "pass example" in {
      Day1.part2(exampleData) shouldBe Some(241861950)
    }

    "produce result" in {
      Day1.part2(inputData) shouldBe Some(65656536)
    }
  }
}

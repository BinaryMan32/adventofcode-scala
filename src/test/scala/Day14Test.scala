package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import com.fivebytestudios.wildfreddy.Day14.Mask
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day14Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day14"
  private val exampleData = getResourceLines("example")
  private val example2Data = getResourceLines("example2")
  private val inputData = getResourceLines("input")

  "Mask.xCombinations" should {
    "return only 0" in {
      Mask(0, 0, 0).xCombinations.toSet should contain theSameElementsAs Set(0)
    }

    "return 2 combinations" in {
      Mask(0, 0, 1).xCombinations.toSet should contain theSameElementsAs Set(0, 1)
    }

    "return 4 combinations" in {
      Mask(0, 0, 5).xCombinations.toSet should contain theSameElementsAs Set(0, 1, 4, 5)
    }

    "return biggest combination" in {
      Mask(0, 0, 1L << 35).xCombinations.toSet should contain theSameElementsAs Set(0, 1L << 35)
    }
  }

  "part1" should {
    "pass example" in {
      Day14.part1(exampleData) shouldBe 165
    }

    "produce result" in {
      Day14.part1(inputData) shouldBe 9967721333886L
    }
  }

  "part2" should {
    "pass example" in {
      Day14.part2(example2Data) shouldBe 208
    }

    "produce result" in {
      Day14.part2(inputData) shouldBe 4355897790573L
    }
  }
}

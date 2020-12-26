package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day21Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day21"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day21.part1(exampleData) shouldBe 5
    }

    "produce result" in {
      Day21.part1(inputData) shouldBe 1829
    }
  }

  "part2" should {
    "pass example" in {
      Day21.part2(exampleData) shouldBe "mxmxvkd,sqjhc,fvjkl"
    }

    "produce result" in {
      Day21.part2(inputData) shouldBe "mxkh,gkcqxs,bvh,sp,rgc,krjn,bpbdlmg,tdbcfb"
    }
  }
}

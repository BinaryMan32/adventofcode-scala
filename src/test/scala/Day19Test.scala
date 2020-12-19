package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day19Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day19"
  private val exampleData = getResourceLines("example")
  private val example2Data = getResourceLines("example2")
  private val inputData = getResourceLines("input")

  "part1" should {
    "match leaf" in {
      Day19.part1(
        """0: "a"
          |
          |a
          |""".stripMargin.split("\n").toList) shouldBe 1
    }
    "not match leaf" in {
      Day19.part1(
        """0: "a"
          |
          |b
          |""".stripMargin.split("\n").toList) shouldBe 0
    }
    "match sequence" in {
      Day19.part1(
        """0: 1 2
          |1: "a"
          |2: "b"
          |
          |ab
          |""".stripMargin.split("\n").toList) shouldBe 1
    }
    "not match sequence" in {
      Day19.part1(
        """0: 1 2
          |1: "a"
          |2: "b"
          |
          |ac
          |""".stripMargin.split("\n").toList) shouldBe 0
    }
    "match nested sequence" in {
      Day19.part1(
        """0: 1 2
          |2: 3 4
          |1: "a"
          |3: "b"
          |4: "c"
          |
          |abc
          |""".stripMargin.split("\n").toList) shouldBe 1
    }
    "not match nested sequence" in {
      Day19.part1(
        """0: 1 2
          |2: 3 4
          |1: "a"
          |3: "b"
          |4: "c"
          |
          |abd
          |""".stripMargin.split("\n").toList) shouldBe 0
    }
    "match alternative" in {
      Day19.part1(
        """0: 1 2 | 2 1
          |1: "a"
          |2: "b"
          |
          |ba
          |""".stripMargin.split("\n").toList) shouldBe 1
    }
    "not match alternative" in {
      Day19.part1(
        """0: 1 2 | 2 1
          |1: "a"
          |2: "b"
          |
          |bc
          |""".stripMargin.split("\n").toList) shouldBe 0
    }
    "pass example" in {
      Day19.part1(exampleData) shouldBe 2
    }

    "pass example2" in {
      Day19.part1(example2Data) shouldBe 3
    }

    "produce result" in {
      Day19.part1(inputData) shouldBe 0
    }
  }

  "part2" should {
    "pass example" in {
      Day19.part2(example2Data) shouldBe 12
    }

    "produce result" in {
      Day19.part2(inputData) shouldBe 0
    }
  }
}

package com.fivebytestudios.wildfreddy

import Day17.Coordinate
import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day17Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day17"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "Coordinate.neighbors" should {
    "work in 1 dimension" in {
      Coordinate(Vector(3)).neighbors should contain theSameElementsAs Set(
        Coordinate(Vector(2)),
        Coordinate(Vector(4))
      )
    }

    "work in 2 dimensions" in {
      Coordinate(Vector(1,4)).neighbors should contain theSameElementsAs Set(
        Coordinate(Vector(0, 3)),
        Coordinate(Vector(1, 3)),
        Coordinate(Vector(2, 3)),
        Coordinate(Vector(0, 4)),
        Coordinate(Vector(2, 4)),
        Coordinate(Vector(0, 5)),
        Coordinate(Vector(1, 5)),
        Coordinate(Vector(2, 5))
      )
    }
  }

  "part1" should {
    "pass example" in {
      Day17.part1(exampleData) shouldBe 112
    }

    "produce result" in {
      Day17.part1(inputData) shouldBe 218
    }
  }

  "part2" should {
    "pass example" in {
      Day17.part2(exampleData) shouldBe 848
    }

    "produce result" in {
      Day17.part2(inputData) shouldBe 1908
    }
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import com.fivebytestudios.wildfreddy.Day24.TileNeighbor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day24Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day24"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "TileNeighbor" should {
    import Day24.TileNeighbor._
    "parse correctly" in {
      parseLine("esew") shouldBe List(East, SouthEast, West)
      parseLine("nwwswee") shouldBe List(NorthWest, West, SouthWest, East, East)
      parseLine("neee") shouldBe List(NorthEast, East, East)
    }
  }

  "TileAddress" should {
    import Day24.TileAddress
    import Day24.TileNeighbor._
    "return to itself" in {
      val origin = TileAddress(0, 0)
      origin.move(NorthWest).move(SouthEast) shouldBe origin
      origin.move(NorthEast).move(SouthWest) shouldBe origin
      origin.move(West).move(East) shouldBe origin
      origin.move(East).move(West) shouldBe origin
      origin.move(SouthWest).move(NorthEast) shouldBe origin
      origin.move(SouthEast).move(NorthWest) shouldBe origin
    }

    def resolvePath(path: String): TileAddress =
      TileAddress.resolvePath(TileNeighbor.parseLine(path))

    "make triangles" in {
      resolvePath("nenesese") shouldBe resolvePath("ee")
      resolvePath("sesenene") shouldBe resolvePath("ee")
      resolvePath("nwnwswsw") shouldBe resolvePath("ww")
      resolvePath("swswnwnw") shouldBe resolvePath("ww")
    }

    "make trapezoids" in {
      resolvePath("neneee") shouldBe resolvePath("eenene")
      resolvePath("seseee") shouldBe resolvePath("eesese")
      resolvePath("nwnwww") shouldBe resolvePath("wwnwnw")
      resolvePath("swswww") shouldBe resolvePath("wwswsw")
    }

    "from example" in {
      resolvePath("esew") shouldBe resolvePath("se")
      resolvePath("nwwswee") shouldBe resolvePath("")
    }
  }

  "part1" should {
    "pass example" in {
      Day24.part1(exampleData) shouldBe 10
    }

    "produce result" in {
      Day24.part1(inputData) shouldBe 375
    }
  }

  "part2" should {
    "pass example" in {
      Day24.part2(exampleData) shouldBe 2208
    }

    "produce result" in {
      Day24.part2(inputData) shouldBe 3937
    }
  }
}

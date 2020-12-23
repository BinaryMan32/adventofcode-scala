package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day20Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day20"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")
  import Day20Test._

  "part1" should {
    "parse tile" in {
      val tile = Day20.parseInput(exampleData.take(11)).head
      tile.id shouldBe 2311
      tile.data.size shouldBe 10
      binStr(tile.data.last) shouldBe "0011100111"
      tile.edges.map(e => binStr(e.edge)) shouldBe List(
        "0011010010", "0001011001", "1110011100", "0100111110"
      )
      val state = Day20.State.initial(List(tile))
      state.assembled((0, 0)).edges.map(edgeStr) shouldBe List(
        "0011010010", "0001011001", "1110011100", "0100111110"
      )
      state.border((0, 1)).pattern.map(_.map(e => binStr(e.edge))) shouldBe List(
        None, None, None, Some("1001101000")
      )
    }
    "pass example" in {
      Day20.part1(exampleData) shouldBe 20899048083289L
    }

    "produce result" in {
      Day20.part1(inputData) shouldBe 60145080587029L
    }
  }

  "part2" should {
    "pass example" in {
      Day20.part2(exampleData) shouldBe 0
    }

    "produce result" in {
      Day20.part2(inputData) shouldBe 0
    }
  }
}

object Day20Test {
  def binStr(num: Int): String =
    num.toBinaryString.reverse.padTo(10, '0').reverse
  def edgeStr(edge: Day20.TileEdge): String =
    binStr(edge.edge)
}

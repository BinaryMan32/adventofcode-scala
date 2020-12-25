package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day20Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day20"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "parse tile" in {
      val tile = Day20.parseInput(exampleData.take(11)).head
      tile.id shouldBe 2311
      tile.data.size shouldBe 10
      tile.data.last shouldBe "..###..###"
      tile.oriented.edges.map(_.edge) shouldBe List(
        "..##.#..#.", "...#.##..#", "###..###..", ".#..#####."
      )
      tile.oriented.rotated.data shouldBe
        """.#..#####.
          |.#.####.#.
          |###...#..#
          |#..#.##..#
          |#....#.##.
          |...##.##.#
          |.#...#....
          |#.#.##....
          |##.###.#.#
          |#..##.#...
          |""".stripMargin.split("\n")
      val state = Day20.State.initial(List(tile))
      state.assembled((0, 0)).edges.map(_.edge) shouldBe List(
        "..##.#..#.", "...#.##..#", "###..###..", ".#..#####."
      )
      state.border((0, 1)).pattern.map(_.map(_.edge)) shouldBe List(
        None, None, None, Some("#..##.#...")
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

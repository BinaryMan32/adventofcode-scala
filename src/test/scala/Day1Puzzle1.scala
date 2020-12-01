package com.fivebytestudios.wildfreddy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day1Puzzle1Test extends AnyFlatSpec with Matchers {

  val testInput = """1721
                    |979
                    |366
                    |299
                    |675
                    |1456""".stripMargin

  it should "pass test" in {
    Day1Puzzle1.run(testInput.linesIterator.map(_.toInt).toList) shouldBe Some(514579)
  }

  it should "produce result" in {
    Day1Puzzle1.run(Source.fromResource("2020/day1/input").getLines.map(_.toInt).toList) shouldBe None
  }
}

package com.fivebytestudios.wildfreddy

import helpers.ResourceHelpers

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day13Test extends AnyWordSpec with Matchers with ResourceHelpers {
  override val resourcePath = "day13"
  private val exampleData = getResourceLines("example")
  private val inputData = getResourceLines("input")

  "part1" should {
    "pass example" in {
      Day13.part1(exampleData) shouldBe 295
    }

    "produce result" in {
      Day13.part1(inputData) shouldBe 3966
    }
  }

  "reduceConstraints" should {
    import Day13.{reduceConstraints, Constraint}

    "solve small examples" in {
      reduceConstraints(Constraint(0, 2), Constraint(1, 3)) shouldBe Constraint(4, 6)
      reduceConstraints(Constraint(1, 3), Constraint(0, 2)) shouldBe Constraint(4, 6)
    }
  }

  "part2" should {
    "pass example" in {
      Day13.findConstraintTime("17,x,13,19") shouldBe 3417
      Day13.findConstraintTime("67,7,59,61") shouldBe 754018
      Day13.findConstraintTime("67,x,7,59,61") shouldBe 779210
      Day13.findConstraintTime("67,7,x,59,61") shouldBe 1261476
      Day13.findConstraintTime("1789,37,47,1889") shouldBe 1202161486
      Day13.part2(exampleData) shouldBe 1068781
    }

    "produce result" in {
      Day13.part2(inputData) shouldBe 800177252346225L
    }
  }
}

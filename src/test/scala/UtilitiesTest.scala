package com.fivebytestudios.wildfreddy

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UtilitiesTest extends AnyWordSpec with Matchers {
  "groupLines" should {
    "group nothing" in {
      Utilities.groupLines(List.empty) shouldBe List(List.empty)
    }

    "group one line into one group" in {
      Utilities.groupLines(List("a")) shouldBe List(List("a"))
    }

    "group two lines into one group" in {
      Utilities.groupLines(List("a", "b")) shouldBe List(List("a", "b"))
    }

    "group two delimited lines into two groups" in {
      Utilities.groupLines(List("a", "", "b")) shouldBe List(List("a"), List("b"))
    }
  }
}

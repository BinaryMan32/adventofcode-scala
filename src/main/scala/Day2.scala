package com.fivebytestudios.wildfreddy

object Day2 {
  val regex = raw"(\d+)-(\d+) (\w): (\w+)$$".r

  def part1(input: List[String]): Int = {
    input.count{ case regex(minCount, maxCount, check, data) =>
      val allowed_range = minCount.toInt to maxCount.toInt
      allowed_range.contains(data.count(_ == check.head))
    }
  }

  def part2(input: List[String]): Int = {
    input.count{ case regex(index1, index2, check, data) =>
      val indices = Seq(index1, index2).map(_.toInt - 1)
      val characters = indices.map(i => data(i))
      characters.count(_ == check.head) == 1
    }
  }
}

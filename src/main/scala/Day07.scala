package com.fivebytestudios.wildfreddy

object Day07 {
  val outerBagRegex = raw"(.*?)\s+bags?\s+contain\s+(.*)".r
  val innerBagRegex = raw"(\d+)\s+(.*?)\s+bags?.?".r
  val emptyInnerBagRegex = raw"no other bags.".r
  def getRules(input: List[String]): Map[String, Map[String, Int]] =
    input.map {
      case outerBagRegex(outerColor, remainder) =>
        outerColor -> remainder.split(raw",\s*")
          .filterNot(emptyInnerBagRegex.matches)
          .map {
            case innerBagRegex(count, innerColor) =>
              innerColor -> count.toInt
          }.toMap
    }.toMap

  def recursivelyContains(findColor: String, searchColor: String, rules: Map[String, Map[String, Int]]): Boolean = {
    findColor == searchColor || rules(searchColor).exists { case (color, count) =>
      recursivelyContains(findColor, color, rules)
    }
  }

  val findColor = "shiny gold"

  def part1(input: List[String]): Long = {
    val rules = getRules(input)
    rules
      .keys
      .filterNot(_ == findColor)
      .count(color => recursivelyContains(findColor, color, rules))
  }

  def countBags(findColor: String, rules: Map[String, Map[String, Int]]): Int = {
    rules(findColor).map { case (color, count) =>
      count * (1 + countBags(color, rules))
    }.sum
  }

  def part2(input: List[String]): Long = {
    countBags(findColor, getRules(input))
  }
}

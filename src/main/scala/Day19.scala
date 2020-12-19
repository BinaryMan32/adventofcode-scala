package com.fivebytestudios.wildfreddy

object Day19 {
  sealed trait Rule {
    /**
     * Constructs a regular expression which will match strings specified by the rules graph.
     * Works for part1, but will recurse forever if the rules graph contains a cycle.
     */
    def buildRegex(rules: Map[Int, Rule]): String = ???

    def matches(text: String, rules: Map[Int, Rule]): Boolean =
      matches(text, List.empty, rules)

    def matches(text: String, rest: List[Rule], rules: Map[Int, Rule]): Boolean = ???
  }
  case class RuleAlternatives(alternatives: List[Rule]) extends Rule {
    override def buildRegex(rules: Map[Int, Rule]): String =
      "(?:" + alternatives.map(_.buildRegex(rules)).mkString("|") + ")"

    override def matches(text: String, rest: List[Rule], rules: Map[Int, Rule]): Boolean =
      alternatives.exists(_.matches(text, rest, rules))
  }
  case class RuleSequence(sequence: List[Int]) extends Rule {
    override def buildRegex(rules: Map[Int, Rule]): String =
      sequence.map(rules).map(_.buildRegex(rules)).mkString

    override def matches(text: String, rest: List[Rule], rules: Map[Int, Rule]): Boolean =
      rules(sequence.head).matches(text, sequence.tail.map(rules) ++ rest, rules)
  }
  case class RulePattern(pattern: String) extends Rule {
    override def buildRegex(rules: Map[Int, Rule]): String = pattern

    override def matches(text: String, rest: List[Rule], rules: Map[Int, Rule]): Boolean = {
      if (text.startsWith(pattern)) {
        val textRemainder = text.drop(pattern.length)
        rest.headOption
          .map(_.matches(textRemainder, rest.tail, rules))
          .getOrElse(textRemainder.isEmpty)
      } else {
        false
      }
    }
  }

  private val ruleRegex = """(\d+):\s+(.*?)""".r
  private val leafRegex = """"([^"]+)"""".r
  def parseRule(line: String): (Int, Rule) = line match {
    case ruleRegex(index, remainder) => {
      val rule = remainder match {
        case leafRegex(text) => RulePattern(text)
        case alternatives => RuleAlternatives(
          alternatives.split(""" \| """).toList.map(
            alternative => RuleSequence(
              alternative.split(" ").toList.map(_.toInt)
            )
          )
        )
      }
      index.toInt -> rule
    }
  }

  def parseRules(input: List[String]): Map[Int, Rule] =
    input.map(parseRule).toMap

  def parseInput(input: List[String]): (Map[Int, Rule], List[String]) = {
    val (ruleLines, messages) = input.span(_.nonEmpty)
    (parseRules(ruleLines), messages.drop(1))
  }

  def part1(input: List[String]): Long = {
    val (rules, messages) = parseInput(input)
    messages.count(m => rules(0).matches(m, rules))
  }

  def part2(input: List[String]): Long = {
    val (oldRules, messages) = parseInput(input)
    // replace rules 8: 42 and 11: 42 31 with the following:
    val newRules = parseRules(
      """8: 42 | 42 8
        |11: 42 31 | 42 11 31
        |""".stripMargin.split("\n").toList)
    val rules = oldRules ++ newRules
    messages.count(m => rules(0).matches(m, rules))
  }
}

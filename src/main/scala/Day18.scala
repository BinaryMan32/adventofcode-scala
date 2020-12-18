package com.fivebytestudios.wildfreddy

import scala.util.parsing.combinator.RegexParsers

object Day18 {
  object Calculator1 extends RegexParsers {
    def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
    def term: Parser[Long] = number | "(" ~> expr <~ ")"
    def expr  : Parser[Long] = term ~ rep("+" ~ term | "*" ~ term) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "+" ~ y) => x + y
        case (x, "*" ~ y) => x * y
      }
    }

    def apply(input: String): Long = parseAll(expr, input).get
  }

  def part1(input: List[String]): Long = {
    input.map(Calculator1(_)).sum
  }

  object Calculator2 extends RegexParsers {
    def number: Parser[Long] = """\d+""".r ^^ { _.toLong }
    def term: Parser[Long] = number | "(" ~> expr <~ ")"
    def sum: Parser[Long] = term ~ rep("+" ~ term) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "+" ~ y) => x + y
      }
    }
    def expr : Parser[Long] = sum ~ rep("*" ~ sum) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, "*" ~ y) => x * y
      }
    }

    def apply(input: String): Long = parseAll(expr, input).get
  }

  def part2(input: List[String]): Long = {
    input.map(Calculator2(_)).sum
  }
}

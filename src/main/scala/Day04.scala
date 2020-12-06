package com.fivebytestudios.wildfreddy

object Day04 {
  val regexCm = raw"(\d+)cm".r
  val regexIn = raw"(\d+)in".r
  val regexHcl = raw"#[0-9a-f]{6}".r
  val ecls = "amb blu brn gry grn hzl oth".split(" ").toSet
  val regexPid = raw"[0-9]{9}".r
  val validators: Map[String, String => Boolean] = Map(
    "byr" -> ((byr: String) => (1920 to 2002).contains(byr.toInt)),
    "iyr" -> ((iyr: String) => (2010 to 2020).contains(iyr.toInt)),
    "eyr" -> ((eyr: String) => (2020 to 2030).contains(eyr.toInt)),
    "hgt" -> {
      case regexCm(x) => (150 to 193).contains(x.toInt)
      case regexIn(x) => (59 to 76).contains(x.toInt)
      case _ => false
    },
    "hcl" -> regexHcl.matches,
    "ecl" -> ecls.contains,
    "pid" -> regexPid.matches
  )

  val requiredFields = validators.keySet

  def toTuple(strings: Seq[String]): (String, String) = (strings(0), strings(1))

  def getPassports(input: List[String]): Seq[Map[String, String]] = {
    input
      .map(_.split(" ").filter(_.nonEmpty).toSeq)
      .foldRight(List(Seq.empty[String]))((entries, passports) =>
        if (entries.isEmpty)
          Seq.empty :: passports
        else
          passports.head.appendedAll(entries) :: passports.tail
      )
      .map(_.map(x => toTuple(x.split(":"))).toMap)
  }

  def isPassportValid(passport: Map[String, String]): Boolean = {
    validators.forall{ case (key, validator) =>
      passport.get(key).exists(validator)
    }
  }

  def part1(input: List[String]): Long = {
    getPassports(input)
      .count(passport => requiredFields.forall(passport.contains))
  }

  def part2(input: List[String]): Long = {
    getPassports(input)
      .count(isPassportValid)
  }
}

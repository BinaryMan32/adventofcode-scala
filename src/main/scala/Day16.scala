package com.fivebytestudios.wildfreddy

object Day16 {
  case class FieldValidator(validRanges: Seq[Range]) {
    def isValid(value: Int): Boolean = validRanges.exists(_.contains(value))
  }
  def parseFieldValidator(line: String): FieldValidator = {
    val rangeRegex = raw"(\d+)-(\d+)".r
    FieldValidator(
      line.split(" or ").map {
        case rangeRegex(a, b) => (a.toInt to b.toInt)
      }.toSeq
    )
  }
  case class FieldDefinitions(fieldValidators: Map[String, FieldValidator]) {
    def isFieldValidForAny(field: Int): Boolean =
      fieldValidators.values.exists(validator => validator.isValid(field))
  }
  case class Ticket(fields: Seq[Int]) {
    def invalidValues(fieldDefinitions: FieldDefinitions): Seq[Int] = {
      fields.filterNot(fieldDefinitions.isFieldValidForAny)
    }
  }
  case class Input(fields: FieldDefinitions, your: Ticket, nearby: List[Ticket]) {

    def fieldMultiMapping: Map[String, Set[Int]] = {
      val nearbyValid = nearby.filter(_.invalidValues(fields).isEmpty)
      val nearbyValidSlices =
        your.fields.indices.map(index => nearbyValid.map(_.fields(index)))
      fields.fieldValidators.map {
        case (fieldName, fieldValidator) =>
          fieldName -> nearbyValidSlices.zipWithIndex.collect {
            case (fieldValues, fieldIndex)
              if fieldValues.forall(fieldValidator.isValid) => fieldIndex
          }.toSet
      }
    }

    def fieldMapping: Map[String, Int] =
      uniqueMapping(fieldMultiMapping)
  }

  def uniqueMapping(input: Map[String, Set[Int]]): Map[String, Int] = {
    val (single, remaining) = input.partition(_._2.size == 1)
    val singleMapping = single.map { case (k, v) => k -> v.head }
    val singleFields = singleMapping.values.toSet
    val remainingMapping = remaining.map { case (k, v) => k -> (v -- singleFields) }
    if (remainingMapping.nonEmpty)
      singleMapping ++ uniqueMapping(remainingMapping)
    else
      singleMapping
  }

  def parseTicket(line: String): Ticket = {
    Ticket(line.split(",").map(_.toInt))
  }
  val fieldRegex = raw"([^:]+): (.*)".r
  def parseFieldDefinition(line: String): (String, FieldValidator) = line match {
    case fieldRegex(name, remainder) => name -> parseFieldValidator(remainder)
  }
  def parseInput(input: List[String]): Input = {
    val (fieldDefinitionsRaw, remainder) = input.span(_.nonEmpty)
    fieldDefinitionsRaw.filter(_.nonEmpty)
    Input(
      fields = FieldDefinitions(fieldDefinitionsRaw.map(parseFieldDefinition).toMap),
      your = parseTicket(remainder(2)),
      nearby = remainder.drop(5).map(parseTicket)
    )
  }
  def part1(input: List[String]): Long = {
    val in = parseInput(input)
    in.nearby.map(
      _.invalidValues(in.fields).sum
    ).sum
  }

  def part2(input: List[String]): Long = {
    val in = parseInput(input)
    val fieldUniqueMapping = in.fieldMapping
    println(s"fieldUniqueMapping=${fieldUniqueMapping}")
    in.fieldMapping
      .collect{ case (k, v) if k.startsWith("departure") => v }
      .map(in.your.fields)
      .map(_.toLong)
      .product
  }
}

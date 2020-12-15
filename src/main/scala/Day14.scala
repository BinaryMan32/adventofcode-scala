package com.fivebytestudios.wildfreddy

import scala.collection.immutable.BitSet

object Day14 {
  case class Mask(ones: Long = 0, zeros: Long = 0, xs: Long = 0) {
    def xCombinations: Iterator[Long] =
      BitSet.fromBitMask(Array(xs))
        .subsets
        .map(_.foldLeft(0L)((acc, n) => acc | (1L << n)))

    def apply(value: Long): Long = (value & ~zeros) | ones
  }

  sealed trait Instruction

  case class SetMask(mask: Mask) extends Instruction
  object SetMask {
    val regex = raw"mask = ([01X]+)".r
    def toMask(value: String, keep: Char): Long =
      BigInt(value.map(x => if (x == keep) '1' else '0'), 2).toLong
    def create(mask: String): SetMask = SetMask(
      mask = Mask(
        ones = toMask(mask, '1'),
        zeros = toMask(mask, '0'),
        xs = toMask(mask, 'X')
      )
    )
  }

  case class SetMem(offset: Long, value: Long) extends Instruction
  object SetMem {
    val regex = raw"mem\[([0-9]+)\] = ([0-9]+)".r
    def create(offset: String, value: String): SetMem = SetMem(
      offset = offset.toLong,
      value = value.toLong
    )
  }

  object Instruction {
    def apply(line: String): Instruction = line match {
      case SetMask.regex(mask) => SetMask.create(mask)
      case SetMem.regex(offset, value) => SetMem.create(offset, value)
    }
  }

  case class State(mask: Mask = Mask(), memory: Map[Long, Long] = Map.empty) {
    def runV1(instruction: Instruction): State = instruction match {
      case SetMask(mask) => copy(mask = mask)
      case SetMem(offset, value) => copy(memory = memory.updated(offset, mask(value)))
    }
    def runV2(instruction: Instruction): State = instruction match {
      case SetMask(mask) => copy(mask = mask)
      case SetMem(offset, value) => {
        val baseOffset = (offset | mask.ones) & ~mask.xs
        val offsets = mask.xCombinations.map(baseOffset | _)
        copy(memory = memory ++ offsets.map(_ -> value))
      }
    }
  }

  def part1(input: List[String]): Long = {
    input
      .map(Instruction(_))
      .foldLeft(State())((state, instruction) => state.runV1(instruction))
      .memory
      .values
      .sum
  }

  def part2(input: List[String]): Long = {
    input
      .map(Instruction(_))
      .foldLeft(State())((state, instruction) => state.runV2(instruction))
      .memory
      .values
      .sum
  }
}

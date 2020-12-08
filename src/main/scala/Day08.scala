package com.fivebytestudios.wildfreddy

import scala.annotation.tailrec

object Day08 {
  case class State(pc: Int = 0, executed: Set[Int] = Set.empty, accumulator: Int = 0)
  sealed abstract class Instruction {
    def execute(state: State): State = ???
    def fixed: Option[Instruction] = None
  }
  case class nop(argument: Int) extends Instruction {
    override def execute(state: State): State = {
      state.copy(pc=state.pc+1)
    }

    override def fixed: Option[Instruction] = Some(jmp(argument))
  }
  case class acc(argument: Int) extends Instruction {
    override def execute(state: State): State = {
      state.copy(pc=state.pc + 1, accumulator = state.accumulator + argument)
    }
  }
  case class jmp(argument: Int) extends Instruction {
    override def execute(state: State): State = {
      state.copy(pc=state.pc + argument)
    }

    override def fixed: Option[Instruction] = Some(nop(argument))
  }
  val regex = raw"(\S+)\s+([+-]\d+)".r
  def parseInstructions(input: List[String]): List[Instruction] = {
    input.map {
      case regex("nop", argument) => nop(argument.toInt)
      case regex("acc", argument) => acc(argument.toInt)
      case regex("jmp", argument) => jmp(argument.toInt)
    }
  }

  @tailrec
  def simulateUntilCycle(instructions: List[Instruction], state: State = State()) : State = {
    if (state.executed.contains(state.pc)) {
      state
    } else {
      simulateUntilCycle(instructions, instructions(state.pc)
        .execute(state.copy(executed=state.executed + state.pc)))
    }
  }

  def part1(input: List[String]): Long = {
    simulateUntilCycle(parseInstructions(input)).accumulator
  }

  @tailrec
  def simulateUntilTerminate(instructions: List[Instruction], state: State = State()) : Option[State] = {
    if (state.pc == instructions.size) {
      Some(state)
    } else if (state.executed.contains(state.pc)) {
      None
    } else {
      simulateUntilTerminate(instructions, instructions(state.pc)
        .execute(state.copy(executed=state.executed + state.pc)))
    }
  }

  def part2(input: List[String]): Long = {
    val instructions = parseInstructions(input)
    instructions
      .indices
      .flatMap(i => instructions(i).fixed.map(e => instructions.updated(i, e)))
      .map(simulateUntilTerminate(_))
      .collectFirst {
        case Some(state) => state.accumulator
      }
      .get
  }
}

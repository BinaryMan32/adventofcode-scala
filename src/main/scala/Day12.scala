package com.fivebytestudios.wildfreddy

object Day12 {
  case class Ship(x: Int = 0, y: Int = 0, r: Int = 0) {
    def move(dx: Int = 0, dy: Int = 0): Ship =
      copy(x = x + dx, y = y + dy)
    def run(instruction: Instruction): Ship =
      instruction match {
        case North(dy) => move(dy=dy)
        case South(dy) => move(dy= -dy)
        case East(dx) => move(dx=dx)
        case West(dx) => move(dx= -dx)
        case Left(dr) => copy(r=(r + dr) % 360)
        case Right(dr) => copy(r=(r + 360 - dr) % 360)
        case Forward(d) => r match {
          case 0 => move(dx=d)
          case 90 => move(dy=d)
          case 180 => move(dx= -d)
          case 270 => move(dy= -d)
        }
      }
  }
  case class Waypoint(x: Int = 0, y: Int = 0) {
    def move(dx: Int = 0, dy: Int = 0): Waypoint =
      Waypoint(x = x + dx, y = y + dy)
    def rotate(degrees: Int): Waypoint =
      degrees match {
        case 90 => copy(x = -y, y = x)
        case 180 => copy(x = -x, y = -y)
        case 270 => copy(x = y, y = -x)
      }
  }
  case class State(ship: Ship = Ship(), waypoint: Waypoint) {
    def run(instruction: Instruction): State =
      instruction match {
        case North(dy) => copy(waypoint=waypoint.move(dy=dy))
        case South(dy) => copy(waypoint=waypoint.move(dy= -dy))
        case East(dx) => copy(waypoint=waypoint.move(dx=dx))
        case West(dx) => copy(waypoint=waypoint.move(dx= -dx))
        case Left(dr) => copy(waypoint=waypoint.rotate(dr))
        case Right(dr) => copy(waypoint=waypoint.rotate(360 - dr))
        case Forward(d) => copy(ship=ship.move(dx=d * waypoint.x, dy=d * waypoint.y))
      }
  }

  sealed trait Instruction
  case class North(amount: Int) extends Instruction
  case class South(amount: Int) extends Instruction
  case class East(amount: Int) extends Instruction
  case class West(amount: Int) extends Instruction
  case class Left(amount: Int) extends Instruction
  case class Right(amount: Int) extends Instruction
  case class Forward(amount: Int) extends Instruction
  object Instruction {
    def apply(input: String): Instruction = {
      val (code, argStr) = input.splitAt(1)
      val arg = argStr.toInt
      code(0) match {
        case 'N' => North(arg)
        case 'S' => South(arg)
        case 'E' => East(arg)
        case 'W' => West(arg)
        case 'L' => Left(arg)
        case 'R' => Right(arg)
        case 'F' => Forward(arg)
      }
    }
  }

  def part1(input: List[String]): Long = {
    val ship = input
      .map(Instruction(_))
      .foldLeft(Ship())((ship, instruction) => ship.run(instruction))
    println(ship)
    ship.x.abs + ship.y.abs
  }

  def part2(input: List[String]): Long = {
    val state = input
      .map(Instruction(_))
      .foldLeft(State(waypoint=Waypoint(x=10, y=1)))(
        (state, instruction) => state.run(instruction))
    println(state)
    state.ship.x.abs + state.ship.y.abs
  }
}

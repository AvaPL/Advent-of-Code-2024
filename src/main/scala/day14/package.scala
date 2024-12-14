package io.github.avapl
package day14

case class Position(row: Int, column: Int)

case class Velocity(row: Int, column: Int)

case class Robot(position: Position, velocity: Velocity)

val spaceWidth = 101
val spaceHeight = 103

def calculatePositionAfterSeconds(robot: Robot, seconds: Int) =
  Position(
    row = math.floorMod(robot.position.row + robot.velocity.row * seconds, spaceHeight),
    column = math.floorMod(robot.position.column + robot.velocity.column * seconds, spaceWidth)
  )

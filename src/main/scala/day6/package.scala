package io.github.avapl
package day6

type LabMap = Vector[Vector[LabMapElement]]

enum LabMapElement {
  case Obstacle
  case FreeSpace
}

case class Position(row: Int, column: Int)

sealed abstract class Direction {
  def rotateClockwise: Direction
  def move(position: Position): Position
}

case object Up extends Direction {
  override lazy val rotateClockwise: Direction = Right

  override def move(position: Position): Position =
    position.copy(row = position.row - 1)
}

case object Right extends Direction {
  override lazy val rotateClockwise: Direction = Down

  override def move(position: Position): Position =
    position.copy(column = position.column + 1)
}

case object Down extends Direction {
  override lazy val rotateClockwise: Direction = Left

  override def move(position: Position): Position =
    position.copy(row = position.row + 1)
}

case object Left extends Direction {
  override lazy val rotateClockwise: Direction = Up

  override def move(position: Position): Position =
    position.copy(column = position.column - 1)
}

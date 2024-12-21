package io.github.avapl
package day16

type Maze = Vector[Vector[MazeElement]]
type Start = Position
type End = Position
type Score = Int

enum MazeElement {
  case Wall, FreeSpace
}

case class Position(row: Int, column: Int) {

  def move(direction: Direction): Position = direction match {
    case Up    => copy(row = row - 1)
    case Right => copy(column = column + 1)
    case Down  => copy(row = row + 1)
    case Left  => copy(column = column - 1)
  }
}

sealed trait Direction {
  def rotateClockwise: Direction
  def rotateCounterClockwise: Direction = rotateClockwise.rotateClockwise.rotateClockwise
}

case object Up extends Direction {
  override lazy val rotateClockwise: Direction = Right
}

case object Right extends Direction {
  override lazy val rotateClockwise: Direction = Down
}

case object Down extends Direction {
  override lazy val rotateClockwise: Direction = Left
}

case object Left extends Direction {
  override lazy val rotateClockwise: Direction = Up
}

val stepScore = 1
val rotationScore = 1000

extension (maze: Maze) {
  def elementAt(position: Position): Option[MazeElement] =
    maze.lift(position.row).flatMap(_.lift(position.column))
}

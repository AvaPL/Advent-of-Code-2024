package io.github.avapl
package day20

enum RacetrackElement {
  case Wall, FreeSpace
}

type Racetrack = Vector[Vector[RacetrackElement]]

case class Position(row: Int, column: Int) {

  lazy val adjacentPositions: List[Position] =
    List(
      positionAbove,
      positionBelow,
      positionToLeft,
      positionToRight
    )

  lazy val positionAbove: Position = Position(row - 1, column)
  lazy val positionBelow: Position = Position(row + 1, column)
  lazy val positionToLeft: Position = Position(row, column - 1)
  lazy val positionToRight: Position = Position(row, column + 1)
}

type Start = Position
type End = Position

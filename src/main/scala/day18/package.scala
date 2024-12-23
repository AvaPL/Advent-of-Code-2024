package io.github.avapl
package day18

case class Position(row: Int, column: Int) {

  lazy val adjacentPositions: List[Position] =
    List(
      copy(row = row + 1),
      copy(row = row - 1),
      copy(column = column + 1),
      copy(column = column - 1)
    ).filter(_.isWithinGrid)

  private lazy val isWithinGrid: Boolean =
    row >= 0 &&
      row < gridSize &&
      column >= 0 &&
      column < gridSize
}

val gridSize = 71
val startPosition = Position(0, 0)
val endPosition = Position(gridSize - 1, gridSize - 1)

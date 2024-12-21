package io.github.avapl
package day15

sealed trait Direction
case object Up extends Direction
case object Right extends Direction
case object Down extends Direction
case object Left extends Direction

case class Position(row: Int, column: Int) {

  def move(direction: Direction): Position =
    direction match {
      case Up    => copy(row = row - 1)
      case Right => copy(column = column + 1)
      case Down  => copy(row = row + 1)
      case Left  => copy(column = column - 1)
    }
}

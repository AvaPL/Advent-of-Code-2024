package io.github.avapl
package day15

sealed trait Direction
sealed trait HorizontalDirection extends Direction
case object Left extends HorizontalDirection
case object Right extends HorizontalDirection
sealed trait VerticalDirection extends Direction
case object Up extends VerticalDirection
case object Down extends VerticalDirection

case class Position(row: Int, column: Int) {

  def move(direction: Direction): Position =
    direction match {
      case Up    => copy(row = row - 1)
      case Right => copy(column = column + 1)
      case Down  => copy(row = row + 1)
      case Left  => copy(column = column - 1)
    }
}

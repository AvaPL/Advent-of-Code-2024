package io.github.avapl
package day21

type Code = String

case class Position(row: Int, column: Int) {

  def move(button: DirectionalKeypadButton): Position =
    button match {
      case Up     => copy(row = row - 1)
      case Right  => copy(column = column + 1)
      case Down   => copy(row = row + 1)
      case Left   => copy(column = column - 1)
      case Action => this
    }
}

sealed trait KeypadElement
case object Gap extends KeypadElement

sealed trait NumericKeypadButton extends KeypadElement
case class DigitButton(value: Int) extends NumericKeypadButton

sealed trait DirectionalKeypadButton extends KeypadElement
case object Up extends DirectionalKeypadButton
case object Right extends DirectionalKeypadButton
case object Down extends DirectionalKeypadButton
case object Left extends DirectionalKeypadButton

case object Action extends NumericKeypadButton with DirectionalKeypadButton

type Keypad[T <: KeypadElement] = Map[T, Position]

object Keypad {
  val numeric: Keypad[KeypadElement] = Map(
    DigitButton(7) -> Position(0, 0),
    DigitButton(8) -> Position(0, 1),
    DigitButton(9) -> Position(0, 2),
    DigitButton(4) -> Position(1, 0),
    DigitButton(5) -> Position(1, 1),
    DigitButton(6) -> Position(1, 2),
    DigitButton(1) -> Position(2, 0),
    DigitButton(2) -> Position(2, 1),
    DigitButton(3) -> Position(2, 2),
    Gap -> Position(3, 0),
    DigitButton(0) -> Position(3, 1),
    Action -> Position(3, 2)
  )

  val directional: Keypad[KeypadElement] = Map(
    Gap -> Position(0, 0),
    Up -> Position(0, 1),
    Action -> Position(0, 2),
    Left -> Position(1, 0),
    Down -> Position(1, 1),
    Right -> Position(1, 2)
  )
}

package io.github.avapl
package day21

import scala.collection.mutable

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

type Keypad[KeypadButton <: KeypadElement] = Map[KeypadButton | Gap.type, Position]

object Keypad {
  val numeric: Keypad[NumericKeypadButton] = Map(
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

  val directional: Keypad[DirectionalKeypadButton] = Map(
    Gap -> Position(0, 0),
    Up -> Position(0, 1),
    Action -> Position(0, 2),
    Left -> Position(1, 0),
    Down -> Position(1, 1),
    Right -> Position(1, 2)
  )
}

def calculateShortestSequencesLengths(codes: List[Code], numberOfDirectionalKeypads: Int) = {

  val numericKeypadCache =
    mutable.Map.empty[(NumericKeypadButton, NumericKeypadButton), Long]
  val directionalKeypadsCache =
    mutable.Map.empty[(Int, DirectionalKeypadButton, DirectionalKeypadButton), Long]

  def findShortestSequence(
      from: NumericKeypadButton,
      to: NumericKeypadButton,
      numberOfDirectionalKeypads: Int
  ) = {

    // numeric (0)
    //    ^ robot
    // keypad (1)
    //    ^ robot
    //
    //    .
    //    .
    //    .
    //
    // keypad (n)
    //    ^ me
    def loop(
        directionalKeypadButtons: Vector[DirectionalKeypadButton],
        keypadNumber: Int
    ): Long =
      if (keypadNumber == 0) {
        numericKeypadCache.getOrElseUpdate(
          (from, to),
          calculateButtonsPermutations(Keypad.numeric, from, to)
            .map(nextDirectionalKeypadButtons => loop(nextDirectionalKeypadButtons, keypadNumber + 1))
            .min
        )
      } else if (keypadNumber == numberOfDirectionalKeypads)
        directionalKeypadButtons.length
      else
        (Action +: directionalKeypadButtons.init)
          .zip(directionalKeypadButtons)
          .map { (from, to) =>
            directionalKeypadsCache.getOrElseUpdate(
              (keypadNumber, from, to),
              calculateButtonsPermutations(Keypad.directional, from, to)
                .map(loop(_, keypadNumber + 1))
                .min
            )
          }
          .sum

    loop(directionalKeypadButtons = Vector.empty, keypadNumber = 0)
  }

  codes.map { code =>
    var currentNumericKeypadButton: NumericKeypadButton = Action

    val myDirectionalButtonsLength = codeToNumericButtons(code).foldLeft(0L) {
      (myDirectionalButtonsLength, nextNumericKeypadButton) =>
        val shortestSequenceLength =
          findShortestSequence(currentNumericKeypadButton, nextNumericKeypadButton, numberOfDirectionalKeypads)
        currentNumericKeypadButton = nextNumericKeypadButton
        myDirectionalButtonsLength + shortestSequenceLength
    }

    code -> myDirectionalButtonsLength
  }.toMap
}

private def codeToNumericButtons(code: Code) =
  code.map {
    case 'A'   => Action
    case digit => DigitButton(digit.asDigit)
  }.toList

private def calculateButtonsPermutations[KeypadButton <: KeypadElement](
    keypad: Keypad[KeypadButton],
    from: KeypadButton,
    to: KeypadButton
) = {
  val initialRobotArmPosition = keypad(from)
  val nextRobotArmPosition = keypad(to)
  val rowDistance = nextRobotArmPosition.row - initialRobotArmPosition.row
  val columnDistance = nextRobotArmPosition.column - initialRobotArmPosition.column
  val movementUp = Vector.fill(-rowDistance)(Up)
  val movementRight = Vector.fill(columnDistance)(Right)
  val movementDown = Vector.fill(rowDistance)(Down)
  val movementLeft = Vector.fill(-columnDistance)(Left)
  (movementUp ++ movementRight ++ movementDown ++ movementLeft).permutations
    .filterNot(reachesKeypadGap(keypad, initialRobotArmPosition))
    .map(_ :+ Action)
    .toList
}

private def reachesKeypadGap[KeypadButton <: KeypadElement](
    keypad: Keypad[KeypadButton],
    position: Position
)(movements: Vector[DirectionalKeypadButton]) =
  movements
    .scanLeft(position)(_.move(_))
    .contains(keypad(Gap))

def calculateComplexity(code: Code, shortestSequenceLength: Long) =
  shortestSequenceLength * code.init.toInt

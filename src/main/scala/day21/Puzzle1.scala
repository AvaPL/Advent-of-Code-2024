package io.github.avapl
package day21

@main def puzzle1(): Unit = {
  val codes = PuzzleInputParser.parsedInput
  val shortestSequences = calculateShortestSequences(codes)
  val result = shortestSequences.map(calculateComplexity).sum
  println(result)
}

private def calculateShortestSequences(codes: List[Code]) =
  codes.map { code =>
    var currentNumericKeypadButton: NumericKeypadButton = Action

    val myDirectionalButtons = codeToNumericButtons(code).foldLeft(Vector.empty[DirectionalKeypadButton]) {
      (myDirectionalButtons, nextNumericKeypadButton) =>
        val shortestSequence = findShortestSequence(currentNumericKeypadButton, nextNumericKeypadButton)
        currentNumericKeypadButton = nextNumericKeypadButton
        myDirectionalButtons ++ shortestSequence
    }

    code -> myDirectionalButtons
  }.toMap

// TODO: Debug only, remove
//private def printDirectionalButtons(directionalKeypadButtons: Seq[DirectionalKeypadButton]): Unit =
//  println {
//    directionalKeypadButtons.map {
//      case Action => 'A'
//      case Up     => '^'
//      case Down   => 'v'
//      case Left   => '<'
//      case Right  => '>'
//    }.mkString
//  }

private def codeToNumericButtons(code: Code) =
  code.map {
    case 'A'   => Action
    case digit => DigitButton(digit.asDigit)
  }.toList

private def findShortestSequence(from: NumericKeypadButton, to: NumericKeypadButton) = {

  // numeric (0)
  //    ^ robot
  // keypad (1)
  //    ^ robot
  // keypad (2)
  //    ^ robot
  // keypad (3)
  //    ^ me
  def loop(
      directionalKeypadButtons: Vector[DirectionalKeypadButton],
      keypadNumber: Int
  ): Vector[DirectionalKeypadButton] =
    if (keypadNumber == 0)
      calculateButtonsPermutations(Keypad.numeric, from, to)
        .map(nextDirectionalKeypadButtons => loop(nextDirectionalKeypadButtons, keypadNumber + 1))
        .minBy(_.length)
    else if (keypadNumber == 3)
      directionalKeypadButtons
    else
      (Action +: directionalKeypadButtons.init)
        .zip(directionalKeypadButtons)
        .flatMap { (from, to) =>
          calculateButtonsPermutations(Keypad.directional, from, to)
            .map(loop(_, keypadNumber + 1))
            .minBy(_.length)
        }

  loop(directionalKeypadButtons = Vector.empty, keypadNumber = 0)
}

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

private def calculateComplexity(code: Code, shortestSequence: Vector[DirectionalKeypadButton]) =
  shortestSequence.length * code.init.toInt

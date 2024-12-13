package io.github.avapl
package day13

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Arcade]](day = 13) {

  override protected def parse(string: String): List[Arcade] =
    for {
      block <- string.splitBlocks
    } yield block match {
      case s"""Button A: X+$aMovementX, Y+$aMovementY
Button B: X+$bMovementX, Y+$bMovementY
Prize: X=$prizeX, Y=$prizeY""" =>
        Arcade(
          Button(aMovementX.toInt, aMovementY.toInt),
          Button(bMovementX.toInt, bMovementY.toInt),
          Prize(prizeX.toInt, prizeY.toInt)
        )
    }
}

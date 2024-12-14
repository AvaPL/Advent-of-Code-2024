package io.github.avapl
package day14

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Robot]](day = 14) {

  override protected def parse(string: String): List[Robot] =
    for {
      case s"p=$positionColumn,$positionRow v=$velocityColumn,$velocityRow" <- string.splitLines
    } yield Robot(
      position = Position(positionRow.toInt, positionColumn.toInt),
      velocity = Velocity(velocityRow.toInt, velocityColumn.toInt)
    )
}

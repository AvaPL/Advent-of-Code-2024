package io.github.avapl
package day18

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Position]](day = 18) {

  override protected def parse(string: String): List[Position] =
    string.splitLines.map {
      case s"$row,$column" => Position(row.toInt, column.toInt)
    }
}

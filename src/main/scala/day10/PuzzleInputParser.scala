package io.github.avapl
package day10

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[TopographicMap](day = 10) {

  override protected def parse(string: String): TopographicMap =
    for {
      line <- string.splitLines.toVector
    } yield for {
      height <- line.toVector
    } yield height.asDigit
}

package io.github.avapl
package day7

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Equation]](day = 7) {

  override protected def parse(string: String): List[Equation] =
    for {
      case s"$testValue: $numbers" <- string.splitLines
    } yield Equation(testValue.toLong, numbers.splitBy(" ").map(_.toLong))
}

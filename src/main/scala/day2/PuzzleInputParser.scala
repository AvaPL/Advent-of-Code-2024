package io.github.avapl
package day2

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Report]](day = 2) {

  override protected def parse(string: String): List[Report] =
    for {
      line <- string.splitLines
    } yield for {
      level <- line.splitBy(" ").map(_.toInt)
    } yield level
}

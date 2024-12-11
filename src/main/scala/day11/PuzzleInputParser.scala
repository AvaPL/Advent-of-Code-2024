package io.github.avapl
package day11

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Stone]](day = 11){

  override protected def parse(string: String): List[Stone] =
    string.splitBy(" ").map(_.toLong).map(Stone)
}

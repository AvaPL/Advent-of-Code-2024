package io.github.avapl
package day21

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[Code]](day = 21) {

  override protected def parse(string: Code): List[Code] =
    string.splitLines
}

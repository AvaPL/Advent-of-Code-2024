package io.github.avapl
package day4

import util.InputParser
import util.StringOps.*

type WordSearch = Vector[String]

object PuzzleInputParser extends InputParser[WordSearch](day = 4) {

  override protected def parse(string: String): WordSearch =
    string.splitLines.toVector
}

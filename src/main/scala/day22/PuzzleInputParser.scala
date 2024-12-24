package io.github.avapl
package day22

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[List[SecretNumber]](day = 22) {

  override protected def parse(string: String): List[SecretNumber] =
    string.splitLines.map(_.toLong)
}

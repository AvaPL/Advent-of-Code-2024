package io.github.avapl
package day19

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(List[Towel], List[Pattern])](day = 19) {

  override protected def parse(string: Towel): (List[Towel], List[Pattern]) = {
    val List(towelsString, patternsString) = string.splitBlocks
    val towels = parseTowels(towelsString)
    val patterns = parsePatterns(patternsString)
    (towels, patterns)
  }

  private def parseTowels(string: Towel): List[Towel] =
    string.splitBy(", ")
    
  private def parsePatterns(string: Towel): List[Pattern] =
    string.splitLines
}

package io.github.avapl
package day19

@main def puzzle1(): Unit = {
  val (towels, patterns) = PuzzleInputParser.parsedInput
  val possiblePatterns = patterns.filter(isPossible(towels))
  val result = possiblePatterns.size
  println(result)
}

private def isPossible(towels: List[Towel])(pattern: Pattern) = {
  val regex = towels.mkString("^(", "|", ")*$").r
  regex.matches(pattern)
}

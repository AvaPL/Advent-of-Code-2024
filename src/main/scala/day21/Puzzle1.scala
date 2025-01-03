package io.github.avapl
package day21

@main def puzzle1(): Unit = {
  val codes = PuzzleInputParser.parsedInput
  val shortestSequencesLengths = calculateShortestSequencesLengths(codes, numberOfDirectionalKeypads = 3)
  val result = shortestSequencesLengths.map(calculateComplexity).sum
  println(result)
}

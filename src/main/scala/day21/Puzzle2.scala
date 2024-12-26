package io.github.avapl
package day21

@main def puzzle2(): Unit = {
  val codes = PuzzleInputParser.parsedInput
  val shortestSequencesLengths = calculateShortestSequencesLengths(codes, numberOfDirectionalKeypads = 26)
  val result = shortestSequencesLengths.map(calculateComplexity).sum
  println(result)
}

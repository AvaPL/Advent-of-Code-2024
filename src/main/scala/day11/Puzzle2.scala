package io.github.avapl
package day11

@main def puzzle2(): Unit = {
  val stones = PuzzleInputParser.parsedInput
  val result = countStonesAfterBlinks(stones, blinkCount = 75)
  println(result)
}

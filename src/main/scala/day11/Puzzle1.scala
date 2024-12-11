package io.github.avapl
package day11

@main def puzzle1(): Unit = {
  val stones = PuzzleInputParser.parsedInput
  val result = countStonesAfterBlinks(stones, blinkCount = 25)
  println(result)
}

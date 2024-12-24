package io.github.avapl
package day22

@main def puzzle1(): Unit = {
  val secretNumbers = PuzzleInputParser.parsedInput
  val result = secretNumbers.map(produceToNth(_, n = 2000).last).sum
  println(result)
}

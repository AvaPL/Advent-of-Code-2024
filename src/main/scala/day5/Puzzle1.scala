package io.github.avapl
package day5

@main def puzzle1(): Unit = {
  val (pageOrderingRules, updates) = PuzzleInputParser.parsedInput
  val correctlyOrderedUpdates = updates.filter(isCorrectOrder(pageOrderingRules))
  val middlePages = correctlyOrderedUpdates.map(_.middlePage)
  val result = middlePages.sum
  println(result)
}

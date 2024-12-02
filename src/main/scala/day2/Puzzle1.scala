package io.github.avapl
package day2

@main def puzzle1(): Unit = {
  val reports = PuzzleInputParser.parsedInput
  val result = reports.count(isSafe)
  println(result)
}

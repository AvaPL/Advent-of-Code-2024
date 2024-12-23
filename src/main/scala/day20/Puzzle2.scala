package io.github.avapl
package day20

@main def puzzle2(): Unit = {
  val (racetrack, start, end) = PuzzleInputParser.parsedInput
  val positionToTime = measureTime(racetrack, start, end)
  val result = countCheats(positionToTime, cheatLength = 20, minimumCheatTimeGain = 100)
  println(result)
}

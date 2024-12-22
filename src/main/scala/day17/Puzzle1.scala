package io.github.avapl
package day17

@main def puzzle1(): Unit = {
  val (registers, program) = PuzzleInputParser.parsedInput
  val output = runProgram(registers, program)
  val result = output.mkString(",")
  println(result)
}

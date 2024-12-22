package io.github.avapl
package day17

@main def puzzle1(): Unit = {
  val (registers, program) = PuzzleInputParser.parsedInput
  val output = runProgram(registers, program)
  val result = output.mkString(",")
  println(result)
}

private def runProgram(initialRegisters: Registers, program: Program) = {
  var registers = initialRegisters
  var instructionPointer = 0
  var output = Output.empty

  while (instructionPointer + 1 < program.length) {
    val instructionOpcode: ThreeBit = program(instructionPointer)
    val instruction = Instruction.ofOpcode(instructionOpcode)
    val operand: ThreeBit = program(instructionPointer + 1)
    val EvaluationResult(newRegisters, instructionPointerChange, newOutput) = instruction.evaluate(registers, operand)
    registers = newRegisters
    instructionPointer = instructionPointerChange(instructionPointer)
    output = output ++ newOutput
  }

  output
}

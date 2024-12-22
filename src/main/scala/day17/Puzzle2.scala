package io.github.avapl
package day17

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val (_, program) = PuzzleInputParser.parsedInput
  val result = findQuineRegisterA(program)
  println(result)
}

// Instruction 0: 2 (bst), Operand: 4 (Register A) -> Register B = Register A & 0b111          -> take last 3 bits of Register A
// Instruction 1: 1 (bxl), Operand: 2 (2)          -> Register B = Register B xor 0b010        -> flip the second bit
// Instruction 2: 7 (cdv), Operand: 5 (Register B) -> Register C = Register A >> Register B    -> shift Register A by 0 to 7 bits (makes backtracking required as up to 10 bits are significant)
// Instruction 3: 4 (bxc), Operand: 5 (ignored)    -> Register B = Register B xor Register C   -> flip bits of Register B based on Register A shifted by 0 to 7 bits (also makes backtracking required)
// Instruction 4: 0 (adv), Operand: 3 (3)          -> Register A = Register A >> 3             -> remove last 3 bits of Register A
// Instruction 5: 1 (bxl), Operand: 7 (7)          -> Register B = Register B xor 0b111        -> flip last 3 bits of Register B
// Instruction 6: 5 (out), Operand: 5 (Register B) -> Output     = Register B & 0b111          -> output the last 3 bits of Register B
// Instruction 7: 3 (jnz), Operand: 0 (0)          -> Jump to Instruction 0 if Register A != 0
private def findQuineRegisterA(program: Program) = {
  var quineRegisterA = Option.empty[Long]
  val potentialRegisterAValuesWithOutputMatchLength = mutable.Queue.empty[(Long, Int)]
  potentialRegisterAValuesWithOutputMatchLength.enqueue((0L, 0))

  while (potentialRegisterAValuesWithOutputMatchLength.nonEmpty) {
    val (potentialRegisterAValue, outputMatchLength) = potentialRegisterAValuesWithOutputMatchLength.dequeue()
    if (outputMatchLength == program.length)
      quineRegisterA = Some(quineRegisterA.getOrElse(potentialRegisterAValue).min(potentialRegisterAValue))
    else
      (0 to 7).map((potentialRegisterAValue << 3) + _).foreach { nextPotentialRegisterAValue =>
        val initialRegisters = Registers(nextPotentialRegisterAValue, 0, 0)
        val output = runProgram(initialRegisters, program)
        if (output == program.takeRight(outputMatchLength + 1))
          potentialRegisterAValuesWithOutputMatchLength.enqueue((nextPotentialRegisterAValue, outputMatchLength + 1))
      }
  }

  quineRegisterA.get
}

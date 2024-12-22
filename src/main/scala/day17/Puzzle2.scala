package io.github.avapl
package day17

import java.lang.Long
import scala.collection.mutable

/** @note
  *   This isn't really a solution but an aggregation of (rather buggy and ugly) code that eventually led me to the
  *   solution. It prints the correct answer but it's very far from being universal or efficient.
  */
@main def puzzle2(): Unit = {
  val (_, program) = PuzzleInputParser.parsedInput
  val rightmostOctets = findRightmostOctets(program)
  val result = {
    for {
      i <- 0 to 7
      j <- 0 to 7
      k <- 0 to 7
      l <- 0 to 7
      m <- 0 to 7
      n <- 0 to 7
      o <- 0 to 7
      p <- 0 to 7
    } yield List(i, j, k, l, m, n, o, p)
  }.view
    .map { leftmostOctets =>
      Long.parseLong((leftmostOctets ++ rightmostOctets).mkString, 8)
    }
    .find { registerA =>
      val output = runProgram(Registers(registerA, 0, 0), program)
      output == program
    }
    .get
  println(result)
}

// Instruction 0: 2 (bst), Operand: 4 (Register A) -> Register B = Register A & 0b111          -> take last 3 bits of Register A
// Instruction 1: 1 (bxl), Operand: 2 (2)          -> Register B = Register B xor 0b010        -> flip the second bit
// Instruction 2: 7 (cdv), Operand: 5 (Register B) -> Register C = Register A >> Register B    -> (hard) shift Register A by 0 to 7 bits
// Instruction 3: 4 (bxc), Operand: 5 (ignored)    -> Register B = Register B xor Register C   -> (hard) flip bits of Register B based on Register A shifted by 0 to 7 bits
// Instruction 4: 0 (adv), Operand: 3 (3)          -> Register A = Register A >> 3             -> remove last 3 bits of Register A
// Instruction 5: 1 (bxl), Operand: 7 (7)          -> Register B = Register B xor 0b111        -> flip last 3 bits of Register B
// Instruction 6: 5 (out), Operand: 5 (Register B) -> Output = Register B & 0b111              -> output the last 3 bits of Register B
// Instruction 7: 3 (jnz), Operand: 0 (0)          -> Jump to Instruction 0 if Register A != 0
private def findRightmostOctets(program: Program) = {
  val noLoopProgram = program.init /* omit jnz which creates a loop */

  var partialSolution = Option.empty[BigInt]
  val queue = mutable.PriorityQueue.empty[(Output, String, String)](Ordering.by {
    case (remainingOutput, _, _) => -remainingOutput.length
  })

  queue.enqueue((program.take(9), "", ""))

  while (partialSolution.isEmpty) {
    val (remainingOutput, registerABitsSuffix, registerABits) = queue.dequeue()
    remainingOutput match {
      case Vector() =>
        partialSolution = Some(BigInt(registerABits, 2))
      case desiredOutput +: remainingOutput =>
        (0 to Integer.parseInt("1" * (10 - registerABitsSuffix.length), 2))
          .map(prefix => Long.parseLong(s"${prefix.toBinaryString}$registerABitsSuffix", 2))
          .map(runNoLoopProgram(_, noLoopProgram))
          .collect {
            case (output, newRegisterABits, willContinueLoop)
                if output == desiredOutput && remainingOutput.nonEmpty == willContinueLoop =>
              (
                newRegisterABits.dropRight(3),
                newRegisterABits.stripSuffix(registerABitsSuffix) + registerABits
              )
          }
          .foreach {
            case (newRegisterABitsSuffix, newRegisterABits) =>
              if (partialSolution.forall(BigInt(newRegisterABits, 2) < _))
                queue.enqueue((remainingOutput, newRegisterABitsSuffix, newRegisterABits))
          }
    }
  }

  partialSolution.get.toString(8).drop(3).map(_.asDigit)
}

private def runNoLoopProgram(registerA: Long, program: Program) = {
  var registers = Registers(registerA, 0, 0)
  var instructionPointer = 0
  var output = Output.empty
  var registerAShift = 0

  while (instructionPointer + 1 < program.length) {
    val instructionOpcode: ThreeBit = program(instructionPointer)
    val instruction = Instruction.ofOpcode(instructionOpcode)
    if (instruction == cdv) // check cdv bit shift to determine significant bits
      registerAShift = registers.B.toInt
    val operand: ThreeBit = program(instructionPointer + 1)
    val EvaluationResult(newRegisters, instructionPointerChange, newOutput) = instruction.evaluate(registers, operand)
    registers = newRegisters
    instructionPointer = instructionPointerChange(instructionPointer)
    output = output ++ newOutput
  }

  val registerABinary = Long.toBinaryString(registerA)
  val loop1IndexFromRight = registerABinary.reverse.drop(3).indexOf('1')
  val significantBits = registerAShift.max(loop1IndexFromRight + 1) + 3
  val willContinueLoop = loop1IndexFromRight >= 0
  // (output, significantBits, willContinueLoop)
  (
    output.head,
    registerABinary.reverse.padTo(significantBits, '0').reverse.takeRight(significantBits),
    willContinueLoop
  )
}

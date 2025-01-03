package io.github.avapl
package day17

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Registers, Program)](day = 17) {

  override protected def parse(string: String): (Registers, Program) =
    string match {
      case s"""Register A: $registerA
Register B: $registerB
Register C: $registerC

Program: $program""" =>
        (
          Registers(registerA.toLong, registerB.toLong, registerC.toLong),
          program.splitBy(",").toVector.map(_.toThreeBit)
        )
    }
}

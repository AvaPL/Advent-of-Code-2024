package io.github.avapl
package day17

type ThreeBit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

extension (int: Int) {
  def toThreeBit: ThreeBit = int.asInstanceOf[ThreeBit]
}

extension (string: String) {
  def toThreeBit: ThreeBit = string.toInt.toThreeBit
}

case class Registers(
    A: Int,
    B: Int,
    C: Int
)

type Program = Vector[ThreeBit]

type InstructionPointer = Int
type InstructionPointerChange = InstructionPointer => InstructionPointer

type Output = Vector[ThreeBit]

object Output {
  val empty: Output = Vector.empty

  def apply(threeBit: ThreeBit): Output =
    Vector(threeBit)
}

case class EvaluationResult(
    registers: Registers,
    instructionPointerChange: InstructionPointerChange = _ + 2,
    output: Output = Output.empty
)

sealed trait Instruction {
  def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult
}

object Instruction {
  def ofOpcode(opcode: ThreeBit): Instruction =
    opcode match {
      case 0 => adv
      case 1 => bxl
      case 2 => bst
      case 3 => jnz
      case 4 => bxc
      case 5 => out
      case 6 => bdv
      case 7 => cdv
    }
}

case object adv extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val numerator = registers.A
    val denominator = 1 << ComboOperand(operand).value(registers)
    val result = numerator / denominator
    EvaluationResult(registers.copy(A = result))
  }
}

case object bxl extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val left = registers.B
    val right = LiteralOperand(operand).value
    val result = left ^ right
    EvaluationResult(registers.copy(B = result))
  }
}

case object bst extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val value = ComboOperand(operand).value(registers)
    val result = math.floorMod(value, 8)
    EvaluationResult(registers.copy(B = result))
  }
}

case object jnz extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult =
    if (registers.A == 0)
      EvaluationResult(registers)
    else {
      val targetInstructionPointer = LiteralOperand(operand).value
      EvaluationResult(registers, instructionPointerChange = _ => targetInstructionPointer)
    }
}

case object bxc extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val left = registers.B
    val right = registers.C
    val result = left ^ right
    EvaluationResult(registers.copy(B = result))
  }
}

case object out extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val value = ComboOperand(operand).value(registers)
    val result: ThreeBit = math.floorMod(value, 8).toThreeBit
    EvaluationResult(registers, output = Output(result))
  }
}

case object bdv extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val numerator = registers.A
    val denominator = 1 << ComboOperand(operand).value(registers)
    val result = numerator / denominator
    EvaluationResult(registers.copy(B = result))
  }
}

case object cdv extends Instruction {
  override def evaluate(registers: Registers, operand: ThreeBit): EvaluationResult = {
    val numerator = registers.A
    val denominator = 1 << ComboOperand(operand).value(registers)
    val result = numerator / denominator
    EvaluationResult(registers.copy(C = result))
  }
}

sealed trait Operand

case class LiteralOperand(value: ThreeBit) extends Operand

case class ComboOperand(id: ThreeBit) extends Operand {
  def value(registers: Registers): Int =
    id match {
      case literal @ (0 | 1 | 2 | 3) => literal
      case 4                         => registers.A
      case 5                         => registers.B
      case 6                         => registers.C
      case 7                         => ??? // reserved
    }
}

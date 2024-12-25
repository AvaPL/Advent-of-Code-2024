package io.github.avapl
package day24

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Map[Wire, Boolean], List[LogicGate])](day = 24) {

  override protected def parse(string: String): (Map[Wire, Boolean], List[LogicGate]) = {
    val List(inputsBlock, gatesBlock) = string.splitBlocks
    val inputs = parseInputs(inputsBlock)
    val gates = parseGates(gatesBlock)
    (inputs, gates)
  }

  private def parseInputs(inputsBlock: String) =
    inputsBlock.splitLines.map {
      case s"$name: $value" => Wire(name) -> (value.toInt > 0)
    }.toMap

  private def parseGates(gatesBlock: String) =
    gatesBlock.splitLines.map {
      case s"$input1 $gate $input2 -> $output" =>
        gate match {
          case "AND" => AND(Wire(input1), Wire(input2), Wire(output))
          case "OR"  => OR(Wire(input1), Wire(input2), Wire(output))
          case "XOR" => XOR(Wire(input1), Wire(input2), Wire(output))
        }
    }
}

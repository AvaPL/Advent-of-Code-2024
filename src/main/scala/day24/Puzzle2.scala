package io.github.avapl
package day24

import java.nio.file.Files

type MermaidChart = String

/** The below code is not a solution but a visualization of the input.
  *
  * Sum operation is implemented as a series of full adders. A reference full adder is visualized in [[FullAdder.mmd]].
  * The program below converts the puzzle input to a Mermaid chart (available in [[Puzzle2.mmd]]).
  *
  * The algorithm for finding the swapped wires was as follows:
  *   1. Find next OR gate.
  *   1. Check if the OR gate has 2 inputs which come from AND gates.
  *   1. Check if the OR gate's output goes to one AND gate and one XOR gate.
  *   1. Check if the XOR gate from the previous step has Z wire on the output.
  *   1. Repeat from step 1.
  *
  * If any of the checks fails, a swapped wire is found. There are two minor exceptions:
  *   1. The first byte is output by a half adder (which doesn't have an OR gate).
  *   1. The last byte is output by a carry from the last full adder's OR gate.
  *
  * This way, the swapped wires were found: hqh,mmk,pvb,qdq,vkq,z11,z24,z38
  *
  * @note
  *   Mermaid charts can be rendered via [[https://mermaid.live]].
  */
@main def puzzle2(): Unit = {
  val (inputs, logicGates) = PuzzleInputParser.parsedInput
  val mermaidChart = convertToMermaidChart(inputs, logicGates)
  printMermaidChart(mermaidChart)
  writeMermaidChart(mermaidChart)
}

private def convertToMermaidChart(inputWires: Map[Wire, Boolean], logicGates: List[LogicGate]) = {
  val header =
    """flowchart LR
      |""".stripMargin
  val mermaidInputs = toMermaidInputs(inputWires)
  val mermaidLogicGates = toMermaidLogicGates(logicGates)
  val mermaidOutputs = toMermaidOutputs(logicGates)
  val mermaidLinks = toMermaidLinks(logicGates)
  header + mermaidInputs + mermaidLogicGates + mermaidOutputs + mermaidLinks
}

private def toMermaidInputs(inputs: Map[Wire, Boolean]) = {

  sealed trait Input
  case class X(name: String, id: Int) extends Input
  case class Y(name: String, id: Int) extends Input

  val inputLines = inputs.keys.toList
    .map {
      case Wire(name @ s"x$id") => X(name, id.toInt)
      case Wire(name @ s"y$id") => Y(name, id.toInt)
    }
    .sortBy {
      case X(_, id) => (id, 0)
      case Y(_, id) => (id, 1)
    }
    .map {
      case X(name, id) => s"$name:::x@{ shape: framed-circle }"
      case Y(name, id) => s"$name:::y@{ shape: framed-circle }"
    }

  val classes =
    """classDef x stroke:#ff0
      |classDef y stroke:#0ff
      |""".stripMargin

  inputLines.mkString("\n", "\n", "\n") + classes
}

private def toMermaidLogicGates(logicGates: List[LogicGate]) = {
  val logicGateLines = logicGates.zipWithIndex.map { (gate, index) =>
    gate match {
      case AND(input1, input2, output) =>
        s"""gate$index:::and@{ shape: delay, label: "AND" }"""
      case OR(input1, input2, output) =>
        s"""gate$index:::or@{ shape: delay, label: "OR" }"""
      case XOR(input1, input2, output) =>
        s"""gate$index:::xor@{ shape: delay, label: "XOR" }"""
    }
  }

  val classes =
    """classDef and stroke:#f00
      |classDef xor stroke:#00f
      |classDef or stroke:#0f0
      |""".stripMargin

  logicGateLines.mkString("\n", "\n", "\n") + classes
}

private def toMermaidOutputs(logicGates: List[LogicGate]) = {

  case class Output(name: String, id: Int)

  val outputLines = logicGates
    .map(_.output)
    .collect {
      case Wire(name @ s"z$id") => Output(name, id.toInt)
    }
    .sortBy(_.id)
    .map {
      case Output(name, id) => s"$name:::z@{ shape: framed-circle }"
    }

  val classes =
    """classDef z stroke:#f0f
      |""".stripMargin

  outputLines.mkString("\n", "\n", "\n") + classes
}

private def toMermaidLinks(logicGates: List[LogicGate]) = {
  val indexedLogicGates = logicGates.zipWithIndex

  val inputLinks = indexedLogicGates
    .flatMap {
      case (gate, gateIndex) =>
        List(
          gate.input1,
          gate.input2
        ).map(_.name).collect {
          case inputName if inputName.startsWith("x") || inputName.startsWith("y") =>
            (inputName, gateIndex)
        }
    }
    .sortBy {
      case (s"x$id", _) => (id.toInt, 0)
      case (s"y$id", _) => (id.toInt, 1)
    }
    .map { (inputName, gateIndex) =>
      s"$inputName -- $inputName --> gate$gateIndex"
    }

  val outputLinks = indexedLogicGates
    .flatMap {
      case (gate, gateIndex) =>
        val outputName = gate.output.name
        Option.when(outputName.startsWith("z")) {
          (gateIndex, outputName)
        }
    }
    .sortBy((gateIndex, _) => gateIndex)
    .map { (gateIndex, outputName) =>
      s"gate$gateIndex -- $outputName --> $outputName"
    }

  val innerLinks = indexedLogicGates
    .collect {
      case (gate, index) if !gate.output.name.startsWith("z") => (gate, index)
    }
    .flatMap { (sourceGate, sourceGateIndex) =>
      indexedLogicGates.collect {
        case (targetGate, targetGateIndex)
            if targetGate.input1 == sourceGate.output || targetGate.input2 == sourceGate.output =>
          (sourceGateIndex, sourceGate.output.name, targetGateIndex)
      }
    }
    .sortBy((sourceGateIndex, _, _) => sourceGateIndex)
    .map { (sourceGateIndex, outputName, targetGateIndex) =>
      s"gate$sourceGateIndex -- $outputName --> gate$targetGateIndex"
    }

  (inputLinks ++ outputLinks ++ innerLinks).mkString("\n", "\n", "\n")
}

private def printMermaidChart(mermaidChart: MermaidChart): Unit =
  println(mermaidChart)

private def writeMermaidChart(mermaidChart: MermaidChart): Unit = {
  val tmpFile = Files.createTempFile("Puzzle2", ".mmd")
  Files.writeString(tmpFile, mermaidChart)
  println(s"Mermaid chart written to: $tmpFile")
}

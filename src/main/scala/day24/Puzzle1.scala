package io.github.avapl
package day24

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (inputs, logicGates) = PuzzleInputParser.parsedInput
  val knownValues = simulate(inputs, logicGates)
  val result = determineZNumber(knownValues)
  println(result)
}

private def simulate(inputs: Map[Wire, Boolean], logicGates: List[LogicGate]) = {
  val knownValues = mutable.Map.from(inputs)
  val logicGatesToSimulate = mutable.Queue.from(logicGates)

  while (logicGatesToSimulate.nonEmpty) {
    val logicGate = logicGatesToSimulate.dequeueFirst { gate =>
      knownValues.contains(gate.input1) && knownValues.contains(gate.input2)
    }.get
    knownValues += logicGate.output -> logicGate.evaluate(knownValues(logicGate.input1), knownValues(logicGate.input2))
  }

  knownValues.toMap
}

private def determineZNumber(knownValues: Map[Wire, Boolean]) = {
  val byteString = knownValues.view
    .filterKeys(_.name.startsWith("z"))
    .toVector
    .sortBy {
      case (Wire(s"z$order"), _) => -order.toInt
    }
    .map {
      case (_, value) => if (value) "1" else "0"
    }
    .mkString
  BigInt(byteString, 2)
}

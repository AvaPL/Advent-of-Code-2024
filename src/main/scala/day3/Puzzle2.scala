package io.github.avapl
package day3

@main def puzzle2(): Unit = {
  val input = PuzzleInputParser.parsedInput
  val instructions = parseInstructions(input)
  val result = evaluateInstructionsStack(instructions)
  println(result)
}

private def parseInstructions(input: String) = {
  val instructionsRegex = s"${Mul.regex}|${Do.regex}|${Dont.regex}".r
  instructionsRegex.findAllIn(input).toList.map {
    case Mul.regex(a, b) => Mul(a.toInt, b.toInt)
    case Do.regex()      => Do
    case Dont.regex()    => Dont
  }
}

private def evaluateInstructionsStack(instructions: List[Instruction]) = {
  var mulEnabled = true
  instructions.map {
    case mul: Mul => if (mulEnabled) mul.evaluate else 0
    case Do       => mulEnabled = true; 0
    case Dont     => mulEnabled = false; 0
  }.sum
}

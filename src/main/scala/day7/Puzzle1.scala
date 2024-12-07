package io.github.avapl
package day7

@main def puzzle1(): Unit = {
  val equations = PuzzleInputParser.parsedInput
  val possiblyTrueEquations = equations.filter(isPossiblyTrue)
  val result = possiblyTrueEquations.map(_.testValue).sum
  println(result)
}

private def isPossiblyTrue(equation: Equation): Boolean = {
  def loop(remainingNumbers: List[Long], acc: Long): Boolean =
    remainingNumbers match {
      case Nil                           => acc == equation.testValue
      case _ if acc > equation.testValue => false
      case number :: tail =>
        loop(tail, acc + number) ||
        loop(tail, acc * number)
    }

  loop(equation.numbers, acc = 0)
}

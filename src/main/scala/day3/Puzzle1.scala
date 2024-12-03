package io.github.avapl
package day3

@main def puzzle1(): Unit = {
  val input = PuzzleInputParser.parsedInput
  val muls = parseMul(input)
  val result = muls.map(_.evaluate).sum
  println(result)
}

private def parseMul(input: String) =
  Mul.regex.findAllIn(input).toList.map {
    case Mul.regex(a, b) => Mul(a.toInt, b.toInt)
  }

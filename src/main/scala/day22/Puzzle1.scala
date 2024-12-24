package io.github.avapl
package day22

@main def puzzle1(): Unit = {
  val secretNumbers = PuzzleInputParser.parsedInput
  val result = secretNumbers.map(produceNth(_, n = 2000)).sum
  println(result)
}

private def produceNth(secretNumber: SecretNumber, n: Int) = {
  var result = secretNumber
  (1 to n).foreach { _ =>
    result = produceNext(result)
  }
  result
}

private def produceNext(secretNumber: SecretNumber) = {

  // Calculate the result of multiplying the secret number by 64 = 2^6.
  // Then, mix this result into the secret number (calculate the bitwise XOR of the given value and the secret number).
  // Finally, prune  the secret number (calculate the value of the secret number modulo 16777216 = 2^24).
  val step1 = ((secretNumber << 6) ^ secretNumber) & 0b111111111111111111111111

  // Calculate the result of dividing the secret number by 32 = 2^5. Round the result down to the nearest integer.
  // Then, mix this result into the secret number.
  // Finally, prune the secret number.
  val step2 = ((step1 >> 5) ^ step1) & 0b111111111111111111111111

  // Calculate the result of multiplying the secret number by 2048 = 2^11.
  // Then, mix this result into the secret number.
  // Finally, prune the secret number.
  val step3 = ((step2 << 11) ^ step2) & 0b111111111111111111111111

  step3
}

package io.github.avapl
package day13

@main def puzzle1(): Unit = {
  val arcades = PuzzleInputParser.parsedInput
  val tokenCosts = arcades.map(calculateTokenCost)
  val result = tokenCosts.collect { case Some(cost) => cost }.sum
  println(result)
}

private def calculateTokenCost(arcade: Arcade) = {
  val Arcade(Button(aMovementX, aMovementY), Button(bMovementX, bMovementY), Prize(prizeX, prizeY)) = arcade
  // prizeX = aButtonPresses * aMovementX + bButtonPresses * bMovementX
  // prizeY = aButtonPresses * aMovementY + bButtonPresses * bMovementY
  val determinant = (aMovementX * bMovementY - bMovementX * aMovementY).toDouble
  if (determinant == 0) None
  else {
    val aButtonPresses = (prizeX * bMovementY - prizeY * bMovementX) / determinant
    val bButtonPresses = (-prizeX * aMovementY + prizeY * aMovementX) / determinant
    Option.when(
      aButtonPresses.isWhole &&
        aButtonPresses >= 0 &&
        aButtonPresses <= 100 &&
        bButtonPresses.isWhole &&
        bButtonPresses >= 0 &&
        bButtonPresses <= 100
    )((aButtonPresses * 3 + bButtonPresses).toInt)
  }
}

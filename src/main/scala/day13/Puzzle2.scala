package io.github.avapl
package day13

@main def puzzle2(): Unit = {
  val arcades = PuzzleInputParser.parsedInput
  val adjustedArcades = arcades.map(adjustPrizePosition)
  val tokenCosts = adjustedArcades.map(calculateTokenCost)
  val result = tokenCosts.collect { case Some(cost) => cost }.sum
  println(result)
}

private def adjustPrizePosition(arcade: Arcade) = {
  val adjustedPrize = arcade.prize.copy(
    xPosition = arcade.prize.xPosition + 10000000000000L,
    yPosition = arcade.prize.yPosition + 10000000000000L
  )
  arcade.copy(prize = adjustedPrize)
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
        bButtonPresses.isWhole &&
        bButtonPresses >= 0
    )((aButtonPresses * 3 + bButtonPresses).toLong)
  }
}

package io.github.avapl
package day14

@main def puzzle1(): Unit = {
  val robots = PuzzleInputParser.parsedInput
  val simulationSeconds = 100
  val robotPositionsAfterSimulation = robots.map(calculatePositionAfterSeconds(_, simulationSeconds))
  val result = calculateSafetyFactor(robotPositionsAfterSimulation)
  println(result)
}

private def calculateSafetyFactor(positions: List[Position]) = {
  val rowQuadrantBoundary = spaceHeight / 2
  val columnQuadrantBoundary = spaceWidth / 2
  val quadrants = positions.foldLeft((0, 0, 0, 0)) {
    case (quadrants, position) =>
      if (position.row < rowQuadrantBoundary && position.column < columnQuadrantBoundary)
        quadrants.copy(_1 = quadrants._1 + 1)
      else if (position.row < rowQuadrantBoundary && position.column > columnQuadrantBoundary)
        quadrants.copy(_2 = quadrants._2 + 1)
      else if (position.row > rowQuadrantBoundary && position.column < columnQuadrantBoundary)
        quadrants.copy(_3 = quadrants._3 + 1)
      else if (position.row > rowQuadrantBoundary && position.column > columnQuadrantBoundary)
        quadrants.copy(_4 = quadrants._4 + 1)
      else
        quadrants
  }
  quadrants._1 * quadrants._2 * quadrants._3 * quadrants._4
}

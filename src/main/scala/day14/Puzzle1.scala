package io.github.avapl
package day14

@main def puzzle1(): Unit = {
  val robots = PuzzleInputParser.parsedInput
  val spaceWidth = 101
  val spaceHeight = 103
  val simulationSeconds = 100
  val robotPositionsAfterSimulation = robots.map(calculatePositionAfterSeconds(simulationSeconds, spaceWidth, spaceHeight))
  val result = calculateSafetyFactor(robotPositionsAfterSimulation, spaceWidth, spaceHeight)
  println(result)
}

private def calculatePositionAfterSeconds(seconds: Int, spaceWidth: Int, spaceHeight: Int)(robot: Robot) =
  Position(
    row = math.floorMod(robot.position.row + robot.velocity.row * seconds, spaceHeight),
    column = math.floorMod(robot.position.column + robot.velocity.column * seconds, spaceWidth)
  )

private def calculateSafetyFactor(positions: List[Position], spaceWidth: Int, spaceHeight: Int) = {
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

package io.github.avapl
package day6

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (labMap, guardPosition) = PuzzleInputParser.parsedInput
  val result = countGuardedPositions(labMap, guardPosition)
  println(result)
}

private def countGuardedPositions(labMap: LabMap, initialGuardPosition: Position): Int = {
  var guardPosition = initialGuardPosition
  var guardOrientation: Direction = Up
  val visitedPositions = mutable.Set.empty[Position]
  var isGuardOutsideBorders = false

  while (!isGuardOutsideBorders) {
    visitedPositions += guardPosition
    val nextPosition = guardOrientation.move(guardPosition)
    if (
      nextPosition.row < 0 ||
      nextPosition.column < 0 ||
      nextPosition.row >= labMap.size ||
      nextPosition.column >= labMap.head.size
    )
      isGuardOutsideBorders = true
    else if (labMap(nextPosition.row)(nextPosition.column) == LabMapElement.Obstacle)
      guardOrientation = guardOrientation.rotateClockwise
    else
      guardPosition = nextPosition
  }

  visitedPositions.size
}

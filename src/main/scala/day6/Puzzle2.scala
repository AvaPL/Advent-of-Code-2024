package io.github.avapl
package day6

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val (labMap, guardPosition) = PuzzleInputParser.parsedInput
  val result = possibleNewObstaclePositions(labMap, guardPosition).count { newObstaclePosition =>
    val newLabMap = labMap.updated(
      newObstaclePosition.row,
      labMap(newObstaclePosition.row).updated(newObstaclePosition.column, LabMapElement.Obstacle)
    )
    isGuardMovingInLoop(newLabMap, guardPosition)
  }
  println(result)
}

private def possibleNewObstaclePositions(labMap: LabMap, initialGuardPosition: Position) =
  for {
    (row, rowIndex) <- labMap.zipWithIndex
    (element, columnIndex) <- row.zipWithIndex
    position = Position(rowIndex, columnIndex)
    if element == LabMapElement.FreeSpace && position != initialGuardPosition
  } yield position

private def isGuardMovingInLoop(labMap: LabMap, initialGuardPosition: Position) = {
  var guardPosition = initialGuardPosition
  var guardOrientation: Direction = Up
  val visitedPositionsWithDirection = mutable.Set.empty[(Position, Direction)]
  var isGuardInsideBorders = true
  var hasLoop = false

  while (isGuardInsideBorders && !hasLoop) {
    if (visitedPositionsWithDirection.contains((guardPosition, guardOrientation)))
      hasLoop = true
    else {
      visitedPositionsWithDirection += ((guardPosition, guardOrientation))
      val nextPosition = guardOrientation.move(guardPosition)
      if (
        nextPosition.row < 0 ||
        nextPosition.column < 0 ||
        nextPosition.row >= labMap.size ||
        nextPosition.column >= labMap.head.size
      )
        isGuardInsideBorders = false
      else if (labMap(nextPosition.row)(nextPosition.column) == LabMapElement.Obstacle)
        guardOrientation = guardOrientation.rotateClockwise
      else
        guardPosition = nextPosition
    }
  }

  hasLoop
}

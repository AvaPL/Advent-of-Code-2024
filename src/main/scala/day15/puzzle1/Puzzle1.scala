package io.github.avapl
package day15.puzzle1

import day15.{Direction, Position}
import WarehouseMapElement.*

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (warehouseMap, robotPosition, robotDirections) = PuzzleInputParser.parsedInput
  val warehouseAfterRobotMoves = doRobotMoves(warehouseMap, robotPosition, robotDirections)
  val result = sumGPSCoordinates(warehouseAfterRobotMoves)
  println(result)
}

private def doRobotMoves(
    warehouseMap: WarehouseMap,
    initialRobotPosition: Position,
    robotDirections: List[Direction]
) = {
  val currentWarehouseMap = mutable.ArrayBuffer.from(warehouseMap.map(mutable.ArrayBuffer.from))
  var currentRobotPosition = initialRobotPosition
  val remainingRobotDirections = mutable.Stack.from(robotDirections)

  while (remainingRobotDirections.nonEmpty) {
    val currentDirection = remainingRobotDirections.pop()
    var currentPosition = currentRobotPosition.move(currentDirection)
    var nextFreeSpace = Option.empty[Position]
    var encounteredBox = false
    var encounteredWall = false
    while (nextFreeSpace.isEmpty && !encounteredWall) {
      currentWarehouseMap(currentPosition.row)(currentPosition.column) match {
        case FreeSpace =>
          nextFreeSpace = Some(currentPosition)
        case Box =>
          encounteredBox = true
          currentPosition = currentPosition.move(currentDirection)
        case Wall =>
          encounteredWall = true
      }
    }
    nextFreeSpace.foreach {
      case Position(nextFreeSpaceRow, nextFreeSpaceColumn) =>
        currentRobotPosition = currentRobotPosition.move(currentDirection)
        if (encounteredBox) { // push boxes
          currentWarehouseMap(currentRobotPosition.row)(currentRobotPosition.column) = FreeSpace
          currentWarehouseMap(nextFreeSpaceRow)(nextFreeSpaceColumn) = Box
        }
    }
  }

  currentWarehouseMap.map(_.toVector).toVector
}

private def sumGPSCoordinates(warehouseMap: WarehouseMap) = {
  for {
    (row, rowIndex) <- warehouseMap.zipWithIndex
    (element, columnIndex) <- row.zipWithIndex
    if element == Box
  } yield GPSCoordinate(rowIndex, columnIndex)
}.sum

private def GPSCoordinate(row: Int, column: Int) =
  100 * row + column

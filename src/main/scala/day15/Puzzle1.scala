package io.github.avapl
package day15

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
  def warehouseMapElementAt(position: Position) = currentWarehouseMap(position.row)(position.column)
  var currentRobotPosition = initialRobotPosition
  val remainingRobotDirections = mutable.Stack.from(robotDirections)

  while (remainingRobotDirections.nonEmpty) {
//    println(s"Current robot position: $currentRobotPosition")
    val currentDirection = remainingRobotDirections.pop()
//    println(s"Moving in direction: $currentDirection")
    var currentPosition = currentRobotPosition.move(currentDirection)
    var nextFreeSpace = Option.empty[Position]
    var encounteredBox = false
    var encounteredWall = false
    while (nextFreeSpace.isEmpty && !encounteredWall) {
      warehouseMapElementAt(currentPosition) match {
        case FreeSpace =>
//          println(s"Found a free space at $currentPosition")
          nextFreeSpace = Some(currentPosition)
        case Box =>
//          println(s"Encountered a box at $currentPosition")
          encounteredBox = true
          currentPosition = currentPosition.move(currentDirection)
        case Wall =>
//          println(s"Encountered wall at $currentPosition")
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
//    currentWarehouseMap.foreach { row =>
//      println {
//        row.map {
//          case Wall => '#'
//          case FreeSpace => '.'
//          case Box => 'O'
//        }.mkString
//      }
//    }
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
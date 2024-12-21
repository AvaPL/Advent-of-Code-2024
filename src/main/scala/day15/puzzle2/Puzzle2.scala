package io.github.avapl
package day15.puzzle2

import day15.*

import scala.annotation.tailrec
import scala.collection.mutable

@main def puzzle2(): Unit = {
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

  def canBoxBeMovedVertically(from: Position, direction: Up.type | Down.type): Boolean =
    currentWarehouseMap(from.row)(from.column) match {
      case BoxLeft =>
        val to = from.move(direction)
        val toRight = to.move(Right)
        (currentWarehouseMap(to.row)(to.column), currentWarehouseMap(toRight.row)(toRight.column)) match {
          case (Wall, _)              => false
          case (_, Wall)              => false
          case (FreeSpace, FreeSpace) => true
          case (boxPart1: BoxPart, boxPart2: BoxPart) =>
            canBoxBeMovedVertically(to, direction) &&
            canBoxBeMovedVertically(toRight, direction)
          case (boxPart: BoxPart, _) =>
            canBoxBeMovedVertically(to, direction)
          case (_, boxPart: BoxPart) =>
            canBoxBeMovedVertically(toRight, direction)
        }
      case BoxRight => // check related BoxLeft instead
        canBoxBeMovedVertically(from.move(Left), direction)
    }

  def moveBoxVertically(from: Position, direction: Up.type | Down.type): Unit =
    currentWarehouseMap(from.row)(from.column) match {
      case BoxLeft =>
        val fromRight = from.move(Right)
        val to = from.move(direction)
        val toRight = to.move(Right)
        if (currentWarehouseMap(to.row)(to.column) != FreeSpace)
          moveBoxVertically(to, direction)
        if (currentWarehouseMap(toRight.row)(toRight.column) != FreeSpace)
          moveBoxVertically(toRight, direction)
        currentWarehouseMap(to.row)(to.column) = BoxLeft
        currentWarehouseMap(toRight.row)(toRight.column) = BoxRight
        currentWarehouseMap(from.row)(from.column) = FreeSpace
        currentWarehouseMap(fromRight.row)(fromRight.column) = FreeSpace
      case BoxRight =>
        moveBoxVertically(from.move(Left), direction)
    }

  @tailrec
  def canBoxBeMovedHorizontally(from: Position, direction: Left.type | Right.type): Boolean = {
    val to = from.move(direction)
    val toElement = currentWarehouseMap(to.row)(to.column)
    (currentWarehouseMap(from.row)(from.column), direction) match {
      case (BoxLeft, Left) =>
        toElement == FreeSpace ||
        (toElement == BoxRight && canBoxBeMovedHorizontally(to, direction))
      case (BoxRight, Right) =>
        toElement == FreeSpace ||
        (toElement == BoxLeft && canBoxBeMovedHorizontally(to, direction))
      case (BoxLeft, Right) | (BoxRight, Left) => // check related box instead
        canBoxBeMovedHorizontally(to, direction)
    }
  }

  def moveBoxHorizontally(from: Position, direction: Left.type | Right.type): Unit = {
    val to = from.move(direction)
    if (currentWarehouseMap(to.row)(to.column) != FreeSpace)
      moveBoxHorizontally(to, direction)
    currentWarehouseMap(to.row)(to.column) = currentWarehouseMap(from.row)(from.column)
    currentWarehouseMap(from.row)(from.column) = FreeSpace
  }

  while (remainingRobotDirections.nonEmpty) {
    val currentDirection = remainingRobotDirections.pop()
    val currentPosition = currentRobotPosition.move(currentDirection)

    currentWarehouseMap(currentPosition.row)(currentPosition.column) match {
      case Wall =>
      case FreeSpace =>
        currentRobotPosition = currentPosition
      case box: BoxPart =>
        currentDirection match {
          case currentDirection @ (Up | Down) if canBoxBeMovedVertically(currentPosition, currentDirection) =>
            moveBoxVertically(currentPosition, currentDirection)
            currentRobotPosition = currentPosition
          case currentDirection @ (Right | Left) if canBoxBeMovedHorizontally(currentPosition, currentDirection) =>
            moveBoxHorizontally(currentPosition, currentDirection)
            currentRobotPosition = currentPosition
          case _ =>
        }
    }
  }

  currentWarehouseMap.map(_.toVector).toVector
}

private def sumGPSCoordinates(warehouseMap: WarehouseMap) = {
  for {
    (row, rowIndex) <- warehouseMap.zipWithIndex
    (element, columnIndex) <- row.zipWithIndex
    if element == BoxLeft
  } yield GPSCoordinate(rowIndex, columnIndex)
}.sum

private def GPSCoordinate(row: Int, column: Int) =
  100 * row + column

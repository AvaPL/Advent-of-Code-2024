package io.github.avapl
package day15

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(WarehouseMap, Position, List[Direction])](day = 15) {

  override protected def parse(string: String): (WarehouseMap, Position, List[Direction]) = {
    val List(warehouseMapBlock, robotDirectionsBlock) = string.splitBlocks
    val (warehouseMap, robotPosition) = parseWarehouseMap(warehouseMapBlock)
    val robotDirections = parseRobotDirections(robotDirectionsBlock)
    (warehouseMap, robotPosition, robotDirections)
  }

  private def parseWarehouseMap(warehouseMapBlock: String) = {
    var robotPosition = Option.empty[Position]
    val warehouseMap = for {
      (row, rowIndex) <- warehouseMapBlock.splitLines.zipWithIndex.toVector
    } yield for {
      (element, columnIndex) <- row.zipWithIndex.toVector
    } yield element match {
      case '#' => Wall
      case '.' => FreeSpace
      case 'O' => Box
      case '@' =>
        robotPosition = Some(Position(rowIndex, columnIndex))
        FreeSpace
    }
    (warehouseMap, robotPosition.get)
  }

  private def parseRobotDirections(robotDirectionsBlock: String) =
    robotDirectionsBlock.splitLines.flatten.map {
      case '^' => Up
      case '>' => Right
      case 'v' => Down
      case '<' => Left
    }
}

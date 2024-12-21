package io.github.avapl
package day15.puzzle2

import day15.*
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
      widenedElement <- element match {
        case '#' => Vector.fill(2)(Wall)
        case '.' => Vector.fill(2)(FreeSpace)
        case 'O' => Vector(BoxLeft, BoxRight)
        case '@' =>
          robotPosition = Some(Position(rowIndex, columnIndex * 2))
          Vector.fill(2)(FreeSpace)
      }
    } yield widenedElement
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

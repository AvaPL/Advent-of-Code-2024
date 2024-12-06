package io.github.avapl
package day6

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(LabMap, Position)](day = 6) {

  override protected def parse(string: String): (LabMap, Position) = {
    var guardPosition = Option.empty[Position]
    val labMap = for {
      (line, rowIndex) <- string.splitLines.zipWithIndex.toVector
    } yield for {
      (element, columnIndex) <- line.zipWithIndex.toVector
    } yield element match {
      case '#' => LabMapElement.Obstacle
      case '.' => LabMapElement.FreeSpace
      case '^' =>
        guardPosition = Some(Position(rowIndex, columnIndex))
        LabMapElement.FreeSpace
    }
    (labMap, guardPosition.get)
  }
}

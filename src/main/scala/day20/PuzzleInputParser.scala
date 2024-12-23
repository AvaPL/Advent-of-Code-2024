package io.github.avapl
package day20

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Racetrack, Start, End)](day = 20) {

  override protected def parse(string: String): (Racetrack, Start, End) = {
    var start = Option.empty[Position]
    var end = Option.empty[Position]
    val racetrack = for {
      (row, rowIndex) <- string.splitLines.zipWithIndex.toVector
    } yield for {
      (element, columnIndex) <- row.zipWithIndex.toVector
    } yield element match {
      case '#' => RacetrackElement.Wall
      case '.' => RacetrackElement.FreeSpace
      case 'S' =>
        start = Some(Position(rowIndex, columnIndex))
        RacetrackElement.FreeSpace
      case 'E' =>
        end = Some(Position(rowIndex, columnIndex))
        RacetrackElement.FreeSpace
    }
    (racetrack, start.get, end.get)
  }
}

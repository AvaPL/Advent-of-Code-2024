package io.github.avapl
package day16

import day16.MazeElement.{FreeSpace, Wall}
import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(Maze, Start, End)](day = 16) {

  override protected def parse(string: String): (Maze, Start, End) = {
    var start = Option.empty[Start]
    var end = Option.empty[End]
    val maze = for {
      (row, rowIndex) <- string.splitLines.zipWithIndex.toVector
    } yield for {
      (element, columnIndex) <- row.zipWithIndex.toVector
    } yield element match {
      case '#' => Wall
      case '.' => FreeSpace
      case 'S' =>
        start = Some(Position(rowIndex, columnIndex))
        FreeSpace
      case 'E' =>
        end = Some(Position(rowIndex, columnIndex))
        FreeSpace
    }
    (maze, start.get, end.get)
  }
}

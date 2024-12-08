package io.github.avapl
package day8

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[AntennasMap](day = 8) {

  override protected def parse(string: String): AntennasMap = {
    val lines = string.splitLines
    val antennas = parseAntennas(lines)
    val rowCount = lines.size
    val columnCount = lines.head.length
    AntennasMap(antennas, rowCount, columnCount)
  }

  private def parseAntennas(lines: List[String]) = {
    for {
      (line, row) <- lines.zipWithIndex
      (element, column) <- line.zipWithIndex
    } yield element match {
      case '.'       => None
      case frequency => Some(Antenna(frequency, Position(row, column)))
    }
  }.flatten.toVector
}

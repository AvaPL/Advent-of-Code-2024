package io.github.avapl
package day8

case class AntennasMap(
    antennas: Vector[Antenna],
    rowCount: Int,
    columnCount: Int
)

case class Antenna(frequency: Char, position: Position)

case class Position(row: Int, column: Int)

class AntinodeFinder(
    antennasMap: AntennasMap
)(
    findAntinodes: (
        antenna1: Antenna,
        antenna2: Antenna,
        isPositionValid: Position => Boolean
    ) => Set[Position]
) {

  lazy val antinodes: Set[Position] = {
    val antennasByFrequency = antennasMap.antennas.groupBy(_.frequency)
    antennasByFrequency.values
      .map(findAntinodes)
      .reduce(_ ++ _)
  }

  private def findAntinodes(antennas: Vector[Antenna]): Set[Position] =
    antennas
      .combinations(2)
      .flatMap {
        case Vector(antenna1, antenna2) => findAntinodes(antenna1, antenna2, isPositionValid)
      }
      .toSet

  private def isPositionValid(position: Position): Boolean = {
    val Position(row, column) = position
    row >= 0 && row < antennasMap.rowCount && column >= 0 && column < antennasMap.columnCount
  }
}

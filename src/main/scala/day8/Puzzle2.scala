package io.github.avapl
package day8

@main def puzzle2(): Unit = {
  val antennasMap = PuzzleInputParser.parsedInput
  val antinodes = new AntinodeFinder(antennasMap)(findAntinodes).antinodes
  val result = antinodes.size
  println(result)
}

private def findAntinodes(
    antenna1: Antenna,
    antenna2: Antenna,
    isPositionValid: Position => Boolean
): Set[Position] = {
  val (directionRow, directionColumn) = (
    antenna2.position.row - antenna1.position.row,
    antenna2.position.column - antenna1.position.column
  )
  findAntinodes(antenna1.position, directionRow, directionColumn, isPositionValid) ++
    findAntinodes(antenna2.position, -directionRow, -directionColumn, isPositionValid)
}

private def findAntinodes(
    antennaPosition: Position,
    directionRow: Int,
    directionColumn: Int,
    isPositionValid: Position => Boolean
) =
  Set.unfold(antennaPosition) {
    case Position(row, column) =>
      val nextPosition = Position(row + directionRow, column + directionColumn)
      Option.when(isPositionValid(nextPosition))((nextPosition, nextPosition))
  }

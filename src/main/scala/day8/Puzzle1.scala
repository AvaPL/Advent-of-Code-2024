package io.github.avapl
package day8

@main def puzzle1(): Unit = {
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
  val Position(row1, column1) = antenna1.position
  val Position(row2, column2) = antenna2.position
  val (directionRow, directionColumn) = (row2 - row1, column2 - column1)
  val antinode1 = Position(row1 - directionRow, column1 - directionColumn)
  val antinode2 = Position(row2 + directionRow, column2 + directionColumn)
  Set(antinode1, antinode2).filter(isPositionValid)
}

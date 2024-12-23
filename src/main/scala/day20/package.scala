package io.github.avapl
package day20

import scala.collection.mutable

enum RacetrackElement {
  case Wall, FreeSpace
}

type Racetrack = Vector[Vector[RacetrackElement]]

case class Position(row: Int, column: Int) {

  lazy val adjacentPositions: List[Position] =
    List(
      Position(row - 1, column),
      Position(row + 1, column),
      Position(row, column - 1),
      Position(row, column + 1)
    )
}

type Start = Position
type End = Position

def measureTime(racetrack: Racetrack, start: Start, end: End) = {

  def racetrackElementAt(position: Position): Option[RacetrackElement] =
    racetrack.lift(position.row).flatMap(_.lift(position.column))

  val positionToTime = mutable.Map.empty[Position, Int]
  var currentPosition = Option(start)
  var currentTime = 0

  while (!positionToTime.contains(end)) {
    positionToTime(currentPosition.get) = currentTime
    currentPosition = currentPosition.get.adjacentPositions
      .filterNot(positionToTime.contains)
      .find(racetrackElementAt(_).contains(RacetrackElement.FreeSpace))
    currentTime += 1
  }

  positionToTime.toMap
}

def countCheats(
    positionToTime: Map[Position, Int],
    cheatLength: Int,
    minimumCheatTimeGain: Int
) =
  positionToTime.keys.toList.flatMap { currentPosition =>
    for {
      rowOffset <- -cheatLength to cheatLength
      rowOffsetLength = math.abs(rowOffset)
      columnOffset <- -cheatLength + rowOffsetLength to cheatLength - rowOffsetLength
      columnOffsetLength = math.abs(columnOffset)
      cheatPosition = Position(currentPosition.row + rowOffset, currentPosition.column + columnOffset)
      if positionToTime.contains(cheatPosition)
      cheatLength = rowOffsetLength + columnOffsetLength
      cheatGain = positionToTime(cheatPosition) - positionToTime(currentPosition) - cheatLength
      if cheatGain >= minimumCheatTimeGain
    } yield cheatGain
  }.size

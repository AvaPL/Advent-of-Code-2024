package io.github.avapl
package day20

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (racetrack, start, end) = PuzzleInputParser.parsedInput
  val positionToTime = measureTime(racetrack, start, end)
  val result = countCheats(racetrack, positionToTime, minimumCheatTimeGain = 100)
  println(result)
}

private def measureTime(racetrack: Racetrack, start: Start, end: End) = {

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

private def countCheats(racetrack: Racetrack, positionToTime: Map[Position, Int], minimumCheatTimeGain: Int) = {
  for {
    (row, rowIndex) <- racetrack.zipWithIndex
    (element, columnIndex) <- row.zipWithIndex
    if rowIndex > 0 && rowIndex < racetrack.length - 1 && // exclude top and bottom boundary
      columnIndex > 0 && columnIndex < row.length - 1 && // exclude left and right boundary
      element == RacetrackElement.Wall
  } yield Position(rowIndex, columnIndex)
}
  .flatMap(cheatGain(positionToTime))
  .count(_ >= minimumCheatTimeGain)

private def cheatGain(positionToTime: Map[Position, Int])(cheatPosition: Position) = {
  val verticalGain = for {
    timeAbove <- positionToTime.get(cheatPosition.positionAbove)
    timeBelow <- positionToTime.get(cheatPosition.positionBelow)
  } yield math.abs(timeAbove - timeBelow)
  val horizontalGain = for {
    timeToLeft <- positionToTime.get(cheatPosition.positionToLeft)
    timeToRight <- positionToTime.get(cheatPosition.positionToRight)
  } yield math.abs(timeToLeft - timeToRight)
  verticalGain.orElse(horizontalGain).map(_ - 2)
}

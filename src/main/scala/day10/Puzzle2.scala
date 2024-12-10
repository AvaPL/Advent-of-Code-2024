package io.github.avapl
package day10

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val topographicMap = PuzzleInputParser.parsedInput
  val trailheads = findAnyHeightTrailheads(topographicMap)
  val trailheadsFrom0 = trailheads.flatten.filter(_.height == 0)
  val result = trailheadsFrom0.map(_.score).sum
  println(result)
}

private def findAnyHeightTrailheads(topographicMap: TopographicMap): Vector[Vector[Trailhead]] = {
  val trailheads = mutable.ArrayBuffer.fill(topographicMap.size) {
    mutable.ArrayBuffer.fill(topographicMap.head.size)(Option.empty[Trailhead])
  }
  def trailheadAt(rowIndex: Int, columnIndex: Int): Option[Trailhead] =
    trailheads.lift(rowIndex).flatMap(_.lift(columnIndex)).flatten

  (0 to 9).reverse.foreach { heightToEvaluate =>
    for {
      (row, rowIndex) <- topographicMap.zipWithIndex
      (height, columnIndex) <- row.zipWithIndex
      if height == heightToEvaluate
    } {
      val adjacent1HigherTrailheads = List(
        (rowIndex, columnIndex - 1),
        (rowIndex, columnIndex + 1),
        (rowIndex - 1, columnIndex),
        (rowIndex + 1, columnIndex)
      ).flatMap { (adjacentRowIndex, adjacentColumnIndex) =>
        trailheadAt(adjacentRowIndex, adjacentColumnIndex)
          .filter(_.height == height + 1)
      }
      val score = if (height == 9) 1 else adjacent1HigherTrailheads.map(_.score).sum
      trailheads(rowIndex)(columnIndex) = Some(Trailhead(height, score))
    }
  }

  for row <- trailheads.toVector yield for trailhead <- row.toVector yield trailhead.get
}

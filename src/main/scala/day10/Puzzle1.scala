package io.github.avapl
package day10

@main def puzzle1(): Unit = {
  val topographicMap = PuzzleInputParser.parsedInput
  val trailheads = findTrailheads(topographicMap)
  val result = trailheads.map(_.score).sum
  println(result)
}

private def findTrailheads(topographicMap: TopographicMap) =
  for {
    (row, rowIndex) <- topographicMap.zipWithIndex
    (height, columnIndex) <- row.zipWithIndex
    if height == 0
    trailhead <- evaluateTrailhead(topographicMap, rowIndex, columnIndex)
  } yield trailhead

private def evaluateTrailhead(topographicMap: TopographicMap, rowIndex: Int, columnIndex: Int) = {

  def heightAt(rowIndex: Int, columnIndex: Int): Option[Height] =
    topographicMap.lift(rowIndex).flatMap(_.lift(columnIndex))

  def calculateScore(currentRowIndex: Int, currentColumnIndex: Int): Set[(Int, Int)] = {
    val currentHeight = topographicMap(currentRowIndex)(currentColumnIndex)
    if (currentHeight == 9)
      Set((currentRowIndex, currentColumnIndex))
    else {
      val adjacent1HigherHeights = Set(
        (currentRowIndex, currentColumnIndex - 1),
        (currentRowIndex, currentColumnIndex + 1),
        (currentRowIndex - 1, currentColumnIndex),
        (currentRowIndex + 1, currentColumnIndex)
      ).filter { (adjacentRowIndex, adjacentColumnIndex) =>
        val adjacentHeight = heightAt(adjacentRowIndex, adjacentColumnIndex)
        adjacentHeight.contains(currentHeight + 1)
      }
      adjacent1HigherHeights.flatMap(calculateScore)
    }
  }

  val score = calculateScore(rowIndex, columnIndex).size
  Option.when(score > 0)(Trailhead(0, score)) // if 0 then it's not a trailhead
}

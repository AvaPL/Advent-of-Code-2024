package io.github.avapl
package day12

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val farmMap = PuzzleInputParser.parsedInput
  val regions = determineRegions(farmMap)
  val result = calculateFencePrice(regions)
  println(result)
}

private def determineRegions(farmMap: FarmMap[GardenPlot]) = {

  def gardenPlotAt(rowIndex: Int, columnIndex: Int): Option[GardenPlot] =
    farmMap.lift(rowIndex).flatMap(_.lift(columnIndex))

  val indicesToVisit = mutable.Set.from(getIndicesToVisit(farmMap))

  def determineRegion(
      currentRowIndex: Int,
      currentColumnIndex: Int,
      currentRegion: List[GardenPlotWithBorders]
  ): List[GardenPlotWithBorders] = {
    if (!indicesToVisit.remove((currentRowIndex, currentColumnIndex)))
      currentRegion
    else {
      val gardenPlot = gardenPlotAt(currentRowIndex, currentColumnIndex).get

      val (adjacentIndices, nonBorders) =
        getAdjacentIndicesWithBorder(currentRowIndex, currentColumnIndex).filter {
          case ((adjacentRowIndex, adjacentColumnIndex), _) =>
            gardenPlotAt(adjacentRowIndex, adjacentColumnIndex).contains(gardenPlot)
        }.unzip

      val borders = Border.values.toSet.removedAll(nonBorders)
      val gardenPlotWithBorders = GardenPlotWithBorders(gardenPlot, borders)

      gardenPlotWithBorders :: adjacentIndices.flatMap(determineRegion(_, _, currentRegion))
    }
  }

  val regions = mutable.ListBuffer.empty[List[GardenPlotWithBorders]]
  while (indicesToVisit.nonEmpty) {
    val (nextRowIndex, nextColumnIndex) = indicesToVisit.head
    regions += determineRegion(nextRowIndex, nextColumnIndex, currentRegion = Nil)
  }
  regions.toList
}

private def getIndicesToVisit(farmMap: FarmMap[GardenPlot]) = {
  for {
    (row, rowIndex) <- farmMap.zipWithIndex
    (gardenPlot, columnIndex) <- row.zipWithIndex
  } yield (rowIndex, columnIndex)
}

private def getAdjacentIndicesWithBorder(currentRowIndex: Int, currentColumnIndex: Int) =
  List(
    ((currentRowIndex - 1, currentColumnIndex), Border.Top),
    ((currentRowIndex, currentColumnIndex + 1), Border.Right),
    ((currentRowIndex + 1, currentColumnIndex), Border.Bottom),
    ((currentRowIndex, currentColumnIndex - 1), Border.Left)
  )

private def calculateFencePrice(regions: List[List[GardenPlotWithBorders]]) =
  regions.map { region =>
    region.size * region.map(_.borders.size).sum
  }.sum

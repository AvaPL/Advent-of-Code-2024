package io.github.avapl
package day12

import scala.collection.mutable
import scala.util.chaining.*

type GardenPlot = Char
type FarmMap = Vector[Vector[GardenPlot]]

enum Border {
  case Top, Right, Bottom, Left
}

case class GardenPlotWithBorders(
    gardenPlot: GardenPlot,
    borders: Set[Border]
)

type Region = Vector[Vector[Option[GardenPlotWithBorders]]]

class RegionExtractor(farmMap: FarmMap) {

  lazy val extractRegions: List[Region] = {
    val indicesToVisit = mutable.Set.from(getIndicesToVisit(farmMap))

    def determineRegion(rowIndex: Int, columnIndex: Int): Region = {

      def loop(
          currentRowIndex: Int,
          currentColumnIndex: Int,
          currentRegion: List[((Int, Int), GardenPlotWithBorders)]
      ): List[((Int, Int), GardenPlotWithBorders)] = {
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

          ((currentRowIndex, currentColumnIndex), gardenPlotWithBorders) ::
            adjacentIndices.flatMap(loop(_, _, currentRegion))
        }
      }

      val indicesWithGardenPlots = loop(rowIndex, columnIndex, currentRegion = Nil)
      toRegion(indicesWithGardenPlots)
    }

    val regions = mutable.ListBuffer.empty[Region]
    while (indicesToVisit.nonEmpty) {
      val (nextRowIndex, nextColumnIndex) = indicesToVisit.head
      regions += determineRegion(nextRowIndex, nextColumnIndex)
    }
    regions.toList
  }

  private def getIndicesToVisit(farmMap: FarmMap) = {
    for {
      (row, rowIndex) <- farmMap.zipWithIndex
      (gardenPlot, columnIndex) <- row.zipWithIndex
    } yield (rowIndex, columnIndex)
  }

  private def gardenPlotAt(rowIndex: Int, columnIndex: Int): Option[GardenPlot] =
    farmMap.lift(rowIndex).flatMap(_.lift(columnIndex))

  private def getAdjacentIndicesWithBorder(currentRowIndex: Int, currentColumnIndex: Int) =
    List(
      ((currentRowIndex - 1, currentColumnIndex), Border.Top),
      ((currentRowIndex, currentColumnIndex + 1), Border.Right),
      ((currentRowIndex + 1, currentColumnIndex), Border.Bottom),
      ((currentRowIndex, currentColumnIndex - 1), Border.Left)
    )

  private def toRegion(indicesWithGardenPlots: List[((Int, Int), GardenPlotWithBorders)]) = {
    val region = mutable.ArrayBuffer.fill(farmMap.size)(
      mutable.ArrayBuffer.fill(farmMap.head.size)(Option.empty[GardenPlotWithBorders])
    )

    indicesWithGardenPlots.foreach {
      case ((rowIndex, columnIndex), gardenPlotWithBorders) =>
        region(rowIndex)(columnIndex) = Some(gardenPlotWithBorders)
    }

    region.toVector.map(_.toVector).pipe(removeEmptyRowsAndColumns)
  }

  private def removeEmptyRowsAndColumns(region: Region) =
    region.filterNot(_.forall(_.isEmpty)).transpose.filterNot(_.forall(_.isEmpty)).transpose
}

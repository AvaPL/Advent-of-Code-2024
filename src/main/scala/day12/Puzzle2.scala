package io.github.avapl
package day12

import scala.annotation.tailrec

@main def puzzle2(): Unit = {
  val farmMap = PuzzleInputParser.parsedInput
  val regions = new RegionExtractor(farmMap).extractRegions
  val result = calculateFencePrice(regions)
  println(result)
}

private def calculateFencePrice(regions: List[Region]) =
  regions.map { region =>
    val area = region.flatten.count(_.isDefined)
    val sides = countSides(region)
    area * sides
  }.sum

private def countSides(region: Region) =
  Border.values.map(countSideWithType(region, _)).sum

private def countSideWithType(region: Region, sideType: Border) = {

  @tailrec
  def loop(row: Vector[Option[GardenPlotWithBorders]], sides: Int): Int = {
    def hasSide(gardenPlot: Option[GardenPlotWithBorders]) = gardenPlot.exists(_.borders.contains(sideType))

    row match {
      case Vector() => sides
      case Some(GardenPlotWithBorders(_, borders)) +: tail if borders.contains(sideType) =>
        loop(row.dropWhile(hasSide), sides + 1)
      case _ +: tail =>
        loop(row.dropWhile(!hasSide(_)), sides)
    }
  }

  sideType match {
    case Border.Top | Border.Bottom => region.map(loop(_, sides = 0)).sum
    case Border.Right | Border.Left => region.transpose.map(loop(_, sides = 0)).sum
  }
}

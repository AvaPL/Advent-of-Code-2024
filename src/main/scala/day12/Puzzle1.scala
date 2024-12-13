package io.github.avapl
package day12

@main def puzzle1(): Unit = {
  val farmMap = PuzzleInputParser.parsedInput
  val regions = new RegionExtractor(farmMap).extractRegions
  val result = calculateFencePrice(regions)
  println(result)
}

private def calculateFencePrice(regions: List[Region]) =
  regions.map { region =>
    var area = 0
    var perimeter = 0
    region.flatten.collect {
      case Some(GardenPlotWithBorders(_, borders)) =>
        area += 1
        perimeter += borders.size
    }
    area * perimeter
  }.sum

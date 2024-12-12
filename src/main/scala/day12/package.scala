package io.github.avapl
package day12

type GardenPlot = Char
type FarmMap[GP <: GardenPlot | GardenPlotWithBorders] = Vector[Vector[GP]]

enum Border:
  case Top, Right, Bottom, Left

case class GardenPlotWithBorders(
    gardenPlot: GardenPlot,
    borders: Set[Border]
)

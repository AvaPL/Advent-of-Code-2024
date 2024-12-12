package io.github.avapl
package day12

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[FarmMap[GardenPlot]](day = 12) {

  override protected def parse(string: String): FarmMap[GardenPlot] =
    for {
      line <- string.splitLines.toVector
    } yield for {
      gardenPlot <- line.toVector
    } yield gardenPlot
}

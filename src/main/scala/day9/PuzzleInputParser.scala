package io.github.avapl
package day9

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[Vector[Block]](day = 9) {

  override protected def parse(string: String): Vector[Block] = {
    for {
      (lengthString, index) <- string.zipWithIndex
    } yield {
      val length = lengthString.asDigit
      if (index % 2 == 0) FileBlock(id = index / 2, length)
      else FreeSpaceBlock(length)
    }
  }.toVector
}

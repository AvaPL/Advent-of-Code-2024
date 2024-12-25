package io.github.avapl
package day25

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(List[Lock], List[Key])](day = 25) {

  override protected def parse(string: String): (List[Lock], List[Key]) =
    string.splitBlocks.partitionMap { block =>
      if (block(0) == '#') Left(parseLock(block))
      else Right(parseKey(block))
    }

  private def parseLock(block: String) =
    Lock(toPinHeights(block))

  private def parseKey(block: String) =
    Key(toPinHeights(block))

  private def toPinHeights(block: String) =
    block.splitLines.transpose.map(_.count(_ == '#') - 1)
}

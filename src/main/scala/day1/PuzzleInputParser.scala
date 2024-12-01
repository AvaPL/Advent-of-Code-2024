package io.github.avapl
package day1

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(List[Int], List[Int])](day = 1) {

  override protected def parse(string: String): (List[Int], List[Int]) =
    string.splitLines
      .map(_.splitByRegex("\\s+"))
      .map { case Seq(a, b) =>
        (a.toInt, b.toInt)
      }
      .unzip
}

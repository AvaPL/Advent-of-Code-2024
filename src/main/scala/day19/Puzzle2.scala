package io.github.avapl
package day19

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val (towels, patterns) = PuzzleInputParser.parsedInput
  val result = countOptions(towels, patterns)
  println(result)
}

private def countOptions(towels: List[Towel], patterns: List[Pattern]) = {
  val cache = mutable.Map.empty[Pattern, Long]

  def loop(pattern: Pattern): Long =
    cache.getOrElseUpdate(
      pattern,
      towels
        .collect {
          case towel if pattern.startsWith(towel) =>
            pattern.drop(towel.length)
        }
        .map { remainingPattern =>
          if (remainingPattern.isEmpty) 1
          else loop(remainingPattern)
        }
        .sum
    )

  patterns.map(loop).sum
}

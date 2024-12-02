package io.github.avapl
package day2

@main def puzzle2(): Unit = {
  val reports = PuzzleInputParser.parsedInput
  val result = reports.count(isSafeWithToleration)
  println(result)
}

private def isSafeWithToleration(report: Report) = {
  lazy val reportsWith1RemovedLevel = report.indices.map { i =>
    report.patch(i, Nil, 1)
  }
  isSafe(report) || reportsWith1RemovedLevel.exists(isSafe)
}
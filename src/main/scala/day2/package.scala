package io.github.avapl
package day2

type Level = Int
type Report = List[Level]

def isSafe(report: Report) = 
  (isAllIncreasing(report) || isAllDecreasing(report)) && hasAllDifferencesBetween1And3(report)

private def isAllIncreasing(report: Report) = 
  report == report.sorted

private def isAllDecreasing(report: Report) = 
  report == report.sorted(Ordering.Int.reverse)

private def hasAllDifferencesBetween1And3(report: Report) =
  report.sliding(2).forall { case List(a, b) =>
    (1 to 3).contains(math.abs(a - b))
  }

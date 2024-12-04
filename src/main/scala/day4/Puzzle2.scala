package io.github.avapl
package day4

@main def puzzle2(): Unit = {
  val wordSearch = PuzzleInputParser.parsedInput
  val result = getXAreas(wordSearch).count(isXmas)
  println(result)
}

private def getXAreas(wordSearch: WordSearch): List[WordSearch] = {
  val areaSize = 3
  for {
    i <- 0 to wordSearch.length - areaSize
    j <- 0 to wordSearch.length - areaSize
  } yield wordSearch.slice(i, i + areaSize).map(_.slice(j, j + areaSize))
}.toList

private def isXmas(xArea: WordSearch) = {
  val diagonal1 = s"${xArea(0)(0)}${xArea(1)(1)}${xArea(2)(2)}"
  val diagonal2 = s"${xArea(2)(0)}${xArea(1)(1)}${xArea(0)(2)}"
  isDiagonalMas(diagonal1) && isDiagonalMas(diagonal2)
}

private def isDiagonalMas(diagonal: String) =
  List(diagonal, diagonal.reverse).contains("MAS")

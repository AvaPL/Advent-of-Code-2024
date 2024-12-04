package io.github.avapl
package day4

@main def puzzle2(): Unit = {
  val wordSearch = PuzzleInputParser.parsedInput
  val result = getXAreas(wordSearch).count(isXmas)
  println(result)
}

private def getXAreas(wordSearch: WordSearch): List[WordSearch] = {
  for {
    i <- 0 to wordSearch.length - 3
    j <- 0 to wordSearch.length - 3
  } yield wordSearch.slice(i, i + 3).map(_.slice(j, j + 3))
}.toList

private def isXmas(xArea: WordSearch) = {
  val diagonal1 = s"${xArea(0)(0)}${xArea(1)(1)}${xArea(2)(2)}"
  val diagonal2 = s"${xArea(2)(0)}${xArea(1)(1)}${xArea(0)(2)}"
  List(diagonal1, diagonal2).forall { diagonal =>
    List(diagonal, diagonal.reverse).contains("MAS")
  }
}

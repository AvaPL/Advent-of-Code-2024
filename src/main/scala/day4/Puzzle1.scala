package io.github.avapl
package day4

@main def puzzle1(): Unit = {
  val wordSearch = PuzzleInputParser.parsedInput
  val horizontalXmasCount = countHorizontalXmas(wordSearch)
  val verticalXmasCount = countVerticalXmas(wordSearch)
  val diagonalXmasCount = countDiagonalXmas(wordSearch)
  val result = horizontalXmasCount + verticalXmasCount + diagonalXmasCount
  println(result)
}

private def countHorizontalXmas(wordSearch: WordSearch) =
  wordSearch.map { line =>
    List(line, line.reverse)
      .map("XMAS".r.findAllIn)
      .map(_.size)
      .sum
  }.sum

private def countVerticalXmas(wordSearch: WordSearch) = {
  val transposedWordSearch = wordSearch.transpose.map(_.mkString)
  countHorizontalXmas(transposedWordSearch)
}

private def countDiagonalXmas(search: WordSearch) =
  countLowerLeftUpperRightDiagonalXmas(search) + countUpperLeftLowerRightDiagonalXmas(search)

private def countLowerLeftUpperRightDiagonalXmas(wordSearch: WordSearch) = {
  // Transform square word search 45 degrees clockwise
  val tiltedWordSearch1 = {
    for {
      i <- wordSearch.indices
    } yield for {
      j <- 0 to i
    } yield wordSearch(i - j)(j)
  }.map(_.mkString).toVector
  val tiltedWordSearch2 = {
    for {
      i <- wordSearch.indices.reverse.drop(1)
    } yield for {
      j <- 0 to i
    } yield wordSearch(wordSearch.size - 1 - j)(wordSearch.size - 1 - i + j)
  }.map(_.mkString).toVector
  val tiltedWordSearch = tiltedWordSearch1 ++ tiltedWordSearch2

  countHorizontalXmas(tiltedWordSearch)
}

private def countUpperLeftLowerRightDiagonalXmas(wordSearch: WordSearch) = {
  val reversedWordSearch = wordSearch.map(_.reverse).map(_.mkString)
  countLowerLeftUpperRightDiagonalXmas(reversedWordSearch)
}

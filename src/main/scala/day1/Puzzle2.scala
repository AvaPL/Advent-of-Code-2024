package io.github.avapl
package day1

@main def puzzle2(): Unit = {
  val (list1, list2) = PuzzleInputParser.parsedInput
  val similarityScores = list1.map { id =>
    id * list2.count(_ == id)
  }
  val result = similarityScores.sum
  println(result)
}

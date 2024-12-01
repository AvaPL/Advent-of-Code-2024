package io.github.avapl
package day1

@main def puzzle1(): Unit = {
  val (list1, list2) = PuzzleInputParser.parsedInput
  val distances = list1.sorted.zip(list2.sorted).map(_ - _).map(math.abs)
  val result = distances.sum
  println(result)
}

package io.github.avapl
package day18

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val bytePositions = PuzzleInputParser.parsedInput
  val result = dijkstra(bytePositions.take(1024).toSet)
  println(result)
}

private def dijkstra(bytePositions: Set[Position]) = {
  val costs = mutable.Map.empty[Position, Int]
  val queue = mutable.PriorityQueue.empty[(Position, Int)](Ordering.by { case (_, cost) => cost })
  queue.enqueue((startPosition, 0))

  while (queue.nonEmpty) {
    val (currentPosition, currentCost) = queue.dequeue()
    if (currentCost < costs.getOrElse(currentPosition, Int.MaxValue)) {
      costs(currentPosition) = currentCost
      if (currentPosition != endPosition) {
        val adjacentPositions = currentPosition.adjacentPositions.filterNot(bytePositions.contains)
        queue.enqueue(adjacentPositions.map((_, currentCost + 1))*)
      }
    }
  }

  costs(endPosition)
}

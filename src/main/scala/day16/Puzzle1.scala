package io.github.avapl
package day16

import day16.MazeElement.FreeSpace

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (maze, start, end) = PuzzleInputParser.parsedInput
  val result = dijkstra(maze, start, end)
  println(result)
}

private def dijkstra(maze: Maze, start: Start, end: End) = {
  val distances = mutable.ArrayBuffer.fill(maze.length, maze.head.length)(Map.empty[Direction, Score])
  val queue = mutable.PriorityQueue.empty[(Position, Direction, Score)](Ordering.by { case (_, _, score) => score })
  queue.enqueue((start, Right, 0)) // start facing East

  while (queue.nonEmpty) {
    val (currentPosition, currentDirection, currentScore) = queue.dequeue()
    val currentDistances = distances(currentPosition.row)(currentPosition.column)
    if (currentDistances.get(currentDirection).forall(_ > currentScore)) {
      distances(currentPosition.row)(currentPosition.column) = currentDistances.updated(currentDirection, currentScore)
      if (currentPosition != end)
        queue.enqueue(
          List(
            (currentDirection, currentScore + stepScore),
            (currentDirection.rotateClockwise, currentScore + rotationScore + stepScore),
            (currentDirection.rotateCounterClockwise, currentScore + rotationScore + stepScore)
          ).flatMap {
            case (nextDirection, nextScore) =>
              val nextPosition = currentPosition.move(nextDirection)
              Option.when(maze.elementAt(nextPosition).contains(FreeSpace)) {
                (nextPosition, nextDirection, nextScore)
              }
          }*
        )
    }
  }

  distances(end.row)(end.column).map {
    case (_, score) => score
  }.min
}

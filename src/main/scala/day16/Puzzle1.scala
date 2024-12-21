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
    if (!currentDistances.contains(currentDirection) || currentScore < currentDistances(currentDirection)) {
      distances(currentPosition.row)(currentPosition.column) = currentDistances.updated(currentDirection, currentScore)
      if (currentPosition != end) {
        val nextPositions = List(
          (currentPosition, currentDirection.rotateClockwise, currentScore + 1000),
          (currentPosition, currentDirection.rotateCounterClockwise, currentScore + 1000)
        ) ++ Some(currentPosition.move(currentDirection))
          .filter {
            case Position(row, column) =>
              row >= 0 &&
              row < maze.length &&
              column >= 0 &&
              column < maze.head.length &&
              maze(row)(column) == FreeSpace
          }
          .map((_, currentDirection, currentScore + 1))
        queue.enqueue(nextPositions*)
      }
    }
  }

  distances(end.row)(end.column).map {
    case (_, score) => score
  }.min
}

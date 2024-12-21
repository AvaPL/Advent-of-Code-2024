package io.github.avapl
package day16

import day16.MazeElement.FreeSpace

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val (maze, start, end) = PuzzleInputParser.parsedInput
  val shortestPaths = dijkstraShortestPaths(maze, start, end)
  val result = shortestPaths.toSet.flatten.size
  println(result)
}

private type Path = List[Position]

private def dijkstraShortestPaths(maze: Maze, start: Start, end: End) = {
  val distances = mutable.ArrayBuffer.fill(maze.length, maze.head.length)(Map.empty[Direction, Score])
  val shortestPaths = mutable.ListBuffer.empty[Path]
  val queue = mutable.PriorityQueue.empty[(Position, Direction, Path, Score)](Ordering.by {
    case (_, _, _, score) => score
  })
  queue.enqueue((start, Right, List(start), 0)) // start facing East

  while (queue.nonEmpty) {
    val (currentPosition, currentDirection, currentPath, currentScore) = queue.dequeue()
    val currentDistances = distances(currentPosition.row)(currentPosition.column)
    if (currentDistances.get(currentDirection).forall(_ >= currentScore)) {
      distances(currentPosition.row)(currentPosition.column) = currentDistances.updated(currentDirection, currentScore)
      if (currentPosition == end) {
        val currentSmallestScore = currentDistances.values.minOption.getOrElse(Int.MaxValue)
        if (currentScore < currentSmallestScore) // found a shorter path
          shortestPaths.clear()
        if (currentScore <= currentSmallestScore)
          shortestPaths += currentPath
      } else
        queue.enqueue(
          List(
            (currentDirection, currentScore + stepScore),
            (currentDirection.rotateClockwise, currentScore + rotationScore + stepScore),
            (currentDirection.rotateCounterClockwise, currentScore + rotationScore + stepScore)
          ).flatMap {
            case (nextDirection, nextScore) =>
              val nextPosition = currentPosition.move(nextDirection)
              Option.when(maze.elementAt(nextPosition).contains(FreeSpace)) {
                (nextPosition, nextDirection, nextPosition :: currentPath, nextScore)
              }
          }*
        )
    }
  }

  shortestPaths.toList
}

package io.github.avapl
package day16

import scala.collection.mutable

type Path = List[Position]

@main def puzzle2(): Unit = {
  val (maze, start, end) = PuzzleInputParser.parsedInput
  val shortestPaths = dijkstraShortestPaths(maze, start, end)
  val result = shortestPaths.toSet.flatten.size
  println(result)
}

def dijkstraShortestPaths(maze: Maze, start: Start, end: End) = {

  def mazeElementAt(position: Position): Option[MazeElement] =
    maze.lift(position.row).flatMap(_.lift(position.column))

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
        if (currentScore < currentSmallestScore)
          shortestPaths.clear()
        if (currentScore <= currentSmallestScore)
          shortestPaths += currentPath
      } else {
        val nextPositions = List(
          (currentPosition, currentDirection.rotateClockwise, currentPath, currentScore + 1000),
          (currentPosition, currentDirection.rotateCounterClockwise, currentPath, currentScore + 1000)
        ).filter {
          case (currentPosition, newDirection, _, _) =>
            mazeElementAt(currentPosition.move(newDirection)).contains(MazeElement.FreeSpace)
        } ++ Some(currentPosition.move(currentDirection))
          .filter(mazeElementAt(_).contains(MazeElement.FreeSpace))
          .map { nextPosition =>
            (nextPosition, currentDirection, nextPosition :: currentPath, currentScore + 1)
          }
        queue.enqueue(nextPositions*)
      }
    }
  }

  shortestPaths.toList
}

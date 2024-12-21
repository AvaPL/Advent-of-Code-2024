package io.github.avapl
package day16

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val (maze, start, end) = PuzzleInputParser.parsedInput
  val result = dijkstra(maze, start, end)
  println(result)
}

private def dijkstra(maze: Maze, start: Start, end: End) = {

  def mazeElementAt(position: Position): Option[MazeElement] =
    maze.lift(position.row).flatMap(_.lift(position.column))

  val distances = mutable.ArrayBuffer.fill(maze.length, maze.head.length)(Map.empty[Direction, Score])
  val queue = mutable.PriorityQueue.empty[(Position, Direction, Score)](Ordering.by { case (_, _, score) => score })
  queue.enqueue((start, Right, 0)) // start facing East

  while (queue.nonEmpty) {
    val (currentPosition, currentDirection, currentScore) = queue.dequeue()
    val currentDistances = distances(currentPosition.row)(currentPosition.column)
    if (currentDistances.get(currentDirection).forall(_ > currentScore)) {
      distances(currentPosition.row)(currentPosition.column) = currentDistances.updated(currentDirection, currentScore)
      if (currentPosition != end) {
        val nextPositions = List(
          (currentPosition, currentDirection.rotateClockwise, currentScore + 1000),
          (currentPosition, currentDirection.rotateCounterClockwise, currentScore + 1000)
        ).filter {
          case (currentPosition, newDirection, _) =>
            mazeElementAt(currentPosition.move(newDirection)).contains(MazeElement.FreeSpace)
        } ++ Some(currentPosition.move(currentDirection))
          .filter(mazeElementAt(_).contains(MazeElement.FreeSpace))
          .map((_, currentDirection, currentScore + 1))
        queue.enqueue(nextPositions*)
      }
    }
  }

  distances(end.row)(end.column).map {
    case (_, score) => score
  }.min
}

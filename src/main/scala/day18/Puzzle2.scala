package io.github.avapl
package day18

import scala.collection.mutable

@main def puzzle2(): Unit = {
  val bytePositions = PuzzleInputParser.parsedInput
  val bytePosition = findFirstBlockingByte(bytePositions)
  val result = s"${bytePosition.row},${bytePosition.column}"
  println(result)
}

private def findFirstBlockingByte(bytePositions: List[Position]) =
  bytePositions.inits.toList.reverse.collectFirst {
    case bytePositions if !hasPathToExit(bytePositions.toSet) => bytePositions.last
  }.get

private def hasPathToExit(bytePositions: Set[Position]) = {
  val reachablePositions = mutable.Set.empty[Position]
  val positionsToCheck = mutable.Set.empty[Position]
  positionsToCheck += startPosition

  while (!reachablePositions.contains(endPosition) && positionsToCheck.nonEmpty) {
    val currentPosition = positionsToCheck.head
    positionsToCheck.remove(currentPosition)
    reachablePositions += currentPosition
    val adjacentPositions = currentPosition.adjacentPositions
      .filterNot(bytePositions.contains)
      .filterNot(reachablePositions.contains)
    positionsToCheck.addAll(adjacentPositions)
  }

  reachablePositions.contains(endPosition)
}

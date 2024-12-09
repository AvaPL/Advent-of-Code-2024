package io.github.avapl
package day9

import scala.collection.mutable

@main def puzzle1(): Unit = {
  val blocks = PuzzleInputParser.parsedInput
  val compactedBlocks = compactBlocks(blocks)
  val result = calculateChecksum(compactedBlocks)
  println(result)
}

private def compactBlocks(blocks: Vector[Block]) = {
  val blocksDeque = mutable.ArrayDeque(blocks*)
  val result = mutable.ArrayBuffer[FileBlock]()

  while (blocksDeque.nonEmpty) {
    blocksDeque.removeHead() match {
      case fileBlock: FileBlock =>
        result += fileBlock
      case freeSpaceBlock: FreeSpaceBlock =>
        blocksDeque.removeLastOption().foreach {
          case fileBlock: FileBlock if fileBlock.length == freeSpaceBlock.length =>
            result += fileBlock
          case fileBlock: FileBlock if fileBlock.length < freeSpaceBlock.length =>
            result += fileBlock
            blocksDeque.prepend(FreeSpaceBlock(freeSpaceBlock.length - fileBlock.length))
          case fileBlock: FileBlock if fileBlock.length > freeSpaceBlock.length =>
            result += fileBlock.copy(length = freeSpaceBlock.length)
            blocksDeque.append(fileBlock.copy(length = fileBlock.length - freeSpaceBlock.length))
          case _ =>
            blocksDeque.prepend(freeSpaceBlock)
        }
    }
  }

  result.toVector
}

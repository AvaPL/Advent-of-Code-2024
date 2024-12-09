package io.github.avapl
package day9

@main def puzzle2(): Unit = {
  val blocks = PuzzleInputParser.parsedInput
  val compactedBlocks = compactBlocks(blocks)
  val result = calculateChecksum(compactedBlocks)
  println(result)
}

private def compactBlocks(blocks: Vector[Block]) =
  blocks.foldRight(blocks) {
    case (_: FreeSpaceBlock, compactedBlocks) => compactedBlocks
    case (fileBlock: FileBlock, compactedBlocks) =>
      val fileBlockIndex = compactedBlocks.indexOf(fileBlock)
      val sufficientFreeSpaceBlock = compactedBlocks.zipWithIndex.collectFirst {
        case (freeSpaceBlock: FreeSpaceBlock, sufficientFreeSpaceIndex)
            if sufficientFreeSpaceIndex < fileBlockIndex && freeSpaceBlock.length >= fileBlock.length =>
          (freeSpaceBlock, sufficientFreeSpaceIndex)
      }
      lazy val replacementFreeSpaceBlock = FreeSpaceBlock(fileBlock.length)

      sufficientFreeSpaceBlock match {
        case None => compactedBlocks
        case Some((freeSpaceBlock, freeSpaceBlockIndex)) if fileBlock.length == freeSpaceBlock.length =>
          compactedBlocks
            .updated(fileBlockIndex, replacementFreeSpaceBlock)
            .updated(freeSpaceBlockIndex, fileBlock)
        case Some((freeSpaceBlock, freeSpaceBlockIndex)) => // fileBlock.length < freeSpaceBlock.length
          val newFreeSpaceBlock = FreeSpaceBlock(length = freeSpaceBlock.length - fileBlock.length)
          compactedBlocks
            .updated(fileBlockIndex, replacementFreeSpaceBlock)
            .patch(freeSpaceBlockIndex, Vector(fileBlock, newFreeSpaceBlock), 1)
      }
  }

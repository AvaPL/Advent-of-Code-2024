package io.github.avapl
package day9

sealed trait Block {
  def length: Int
}

case class FileBlock(id: Int, length: Int) extends Block

case class FreeSpaceBlock(length: Int) extends Block

def calculateChecksum(compactedBlocks: Vector[Block]) =
  compactedBlocks.foldLeft((0L, 0L)) {
    case ((index, checksum), block: Block) =>
      val newIndex = index + block.length
      block match {
        case FileBlock(id, _) =>
          val newChecksum = checksum + (index until newIndex).map(_ * id).sum
          (newIndex, newChecksum)
        case _: FreeSpaceBlock =>
          (newIndex, checksum)
      }
  } match {
    case (_, checksum) => checksum
  }

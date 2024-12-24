package io.github.avapl
package day23

import util.InputParser
import util.StringOps.*

import scala.collection.mutable

object PuzzleInputParser extends InputParser[Connections](day = 23) {

  override protected def parse(string: String): Connections = {
    val connections = mutable.Map.empty[Computer, Set[Computer]]
    string.splitLines.foreach {
      case s"$from-$to" =>
        val computerFrom = Computer(from)
        val computerTo = Computer(to)
        connections.updateWith(computerFrom) {
          case Some(computers) => Some(computers + computerTo)
          case None            => Some(Set(computerTo))
        }
        connections.updateWith(computerTo) {
          case Some(computers) => Some(computers + computerFrom)
          case None            => Some(Set(computerFrom))
        }
    }
    connections.toMap
  }
}

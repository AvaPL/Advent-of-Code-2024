package io.github.avapl
package day5

import util.InputParser
import util.StringOps.*

object PuzzleInputParser extends InputParser[(PageOrderingRules, List[Update])](day = 5) {

  override protected def parse(string: String): (PageOrderingRules, List[Update]) = {
    val List(rulesBlock, updatesBlock) = string.splitBlocks
    val pageOrderingRules = parsePageOrderingRules(rulesBlock)
    val updates = parseUpdates(updatesBlock)
    (pageOrderingRules, updates)
  }

  private def parsePageOrderingRules(rulesBlock: String): PageOrderingRules = {
    for {
      case s"$rulePage|$beforePage" <- rulesBlock.splitLines
    } yield (rulePage.toInt, beforePage.toInt)
  }.groupMap {
    case (before, _) => before
  } {
    case (_, page) => page
  }.view
    .mapValues(_.toSet)
    .toMap

  private def parseUpdates(updatesBlock: String): List[Update] =
    for {
      update <- updatesBlock.splitLines
    } yield for {
      page <- update.splitBy(",")
    } yield page.toInt
}

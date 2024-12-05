package io.github.avapl
package day5

import scala.annotation.tailrec

@main def puzzle2(): Unit = {
  val (pageOrderingRules, updates) = PuzzleInputParser.parsedInput
  val incorrectlyOrderedUpdates = updates.filterNot(isCorrectOrder(pageOrderingRules))
  val reorderedUpdates = incorrectlyOrderedUpdates.map(reorderUpdate(pageOrderingRules))
  val middlePages = reorderedUpdates.map(_.middlePage)
  val result = middlePages.sum
  println(result)
}

private def reorderUpdate(pageOrderingRules: PageOrderingRules)(update: Update): Update = {
  @tailrec
  def loop(remainingPages: Set[Page], printedPages: List[Page]): Update = {
    if (remainingPages.isEmpty)
      printedPages
    else { // there's always at least one unrestricted page with regards to the remaining pages
      val pageWithoutRestrictions = remainingPages.find { page =>
        pageOrderingRules.getBeforePages(page).intersect(remainingPages).isEmpty
      }.get
      loop(
        remainingPages = remainingPages - pageWithoutRestrictions,
        printedPages = pageWithoutRestrictions :: printedPages
      )
    }
  }

  loop(remainingPages = update.toSet, printedPages = Nil)
}

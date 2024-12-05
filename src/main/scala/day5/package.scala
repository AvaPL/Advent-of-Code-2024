package io.github.avapl
package day5

import scala.annotation.tailrec

type Page = Int
type Update = List[Page]

extension (update: Update) {
  def middlePage: Page = update(update.size / 2)
}

type PageOrderingRules = Map[Page, Set[Page]]

extension (pageOrderingRules: PageOrderingRules) {
  def getBeforePages(page: Page): Set[Page] =
    pageOrderingRules.getOrElse(page, Set.empty)
}

def isCorrectOrder(pageOrderingRules: PageOrderingRules)(update: Update): Boolean = {
  @tailrec
  def loop(remainingPages: List[Page], printedPages: Set[Page]): Boolean = {
    remainingPages match {
      case Nil => true
      case pageToPrint :: remainingPages =>
        val isPrintingAllowed = pageOrderingRules.getBeforePages(pageToPrint).intersect(printedPages).isEmpty
        if (isPrintingAllowed)
          loop(remainingPages, printedPages = printedPages + pageToPrint)
        else
          false
    }
  }

  loop(update, printedPages = Set.empty)
}

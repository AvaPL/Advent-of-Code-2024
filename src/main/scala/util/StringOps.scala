package io.github.avapl
package util

import scala.util.matching.Regex

object StringOps {

  extension (string: String) {
    def splitByRegex(regex: String): List[String] =
      string.split(regex).toList

    def splitBy(delimiter: String): List[String] = {
      val quotedDelimiter = Regex.quote(delimiter)
      splitByRegex(quotedDelimiter)
    }

    def splitLines: List[String] =
      splitBy("\n")

    def splitBlocks: List[String] =
      splitBy("\n\n")
  }
}
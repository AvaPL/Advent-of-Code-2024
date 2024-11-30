package io.github.avapl
package util

import scala.util.matching.Regex

object StringOps {

  extension (string: String) {
    def splitByRegex(regex: String): Seq[String] =
      string.split(regex).toSeq

    def splitBy(delimiter: String): Seq[String] = {
      val quotedDelimiter = Regex.quote(delimiter)
      splitByRegex(quotedDelimiter)
    }

    def splitLines: Seq[String] =
      splitBy("\n")

    def splitBlocks: Seq[String] =
      splitBy("\n\n")
  }
}
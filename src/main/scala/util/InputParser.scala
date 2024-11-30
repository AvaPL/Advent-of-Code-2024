package io.github.avapl
package util

import scala.io.Source
import scala.util.Using

abstract class InputParser[T](day: Int) {

  protected def parse(string: String): T

  lazy val parsedInput: T = {
    val fileName = s"input/day$day.txt"
    val inputString = Using(Source.fromResource(fileName))(_.mkString).get
    parse(inputString)
  }
}

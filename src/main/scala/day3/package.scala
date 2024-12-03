package io.github.avapl
package day3

import scala.util.matching.Regex

sealed trait Instruction

case class Mul(a: Int, b: Int) extends Instruction {
  lazy val evaluate: Int = a * b
}

object Mul {
  val regex: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
}

case object Do extends Instruction {
  val regex: Regex = """do\(\)""".r
}

case object Dont extends Instruction {
  val regex: Regex = """don't\(\)""".r
}

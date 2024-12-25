package io.github.avapl
package day24

case class Wire(name: String)

sealed trait LogicGate {
  def input1: Wire
  def input2: Wire
  def output: Wire
  def evaluate(input1: Boolean, input2: Boolean): Boolean
}

case class AND(input1: Wire, input2: Wire, output: Wire) extends LogicGate {
  override def evaluate(input1: Boolean, input2: Boolean): Boolean =
    input1 && input2
}

case class OR(input1: Wire, input2: Wire, output: Wire) extends LogicGate {
  override def evaluate(input1: Boolean, input2: Boolean): Boolean =
    input1 || input2
}

case class XOR(input1: Wire, input2: Wire, output: Wire) extends LogicGate {
  override def evaluate(input1: Boolean, input2: Boolean): Boolean =
    input1 ^ input2
}

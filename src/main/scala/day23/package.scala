package io.github.avapl
package day23 

case class Computer(name: String)

type Connections = Map[Computer, Set[Computer]]
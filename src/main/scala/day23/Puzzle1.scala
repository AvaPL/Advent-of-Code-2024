package io.github.avapl
package day23

@main def puzzle1(): Unit = {
  val connections = PuzzleInputParser.parsedInput
  val result = countPotentialHistorianLANParties(connections)
  println(result)
}

private def countPotentialHistorianLANParties(connections: Connections) = {
  val potentialHistorianComputers = connections.keySet.filter(_.name.startsWith("t"))
  potentialHistorianComputers.flatMap { computer1 =>
    connections(computer1).toList.combinations(2).collect {
      case List(computer2, computer3) if connections(computer2).contains(computer3) =>
        Set(computer1, computer2, computer3)
    }
  }.size
}

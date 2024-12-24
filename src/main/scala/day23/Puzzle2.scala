package io.github.avapl
package day23

@main def puzzle2(): Unit = {
  val connections = PuzzleInputParser.parsedInput
  val largestMaximalClique = findLargestMaximalClique(connections)
  val result = password(largestMaximalClique)
  println(result)
}

private def findLargestMaximalClique(connections: Connections) = {
  var largestMaximalClique = Set.empty[Computer]

  def bronKerbosch(r: Set[Computer], p: Set[Computer], x: Set[Computer]): Unit = {
    if (p.isEmpty && x.isEmpty && r.size > largestMaximalClique.size)
      largestMaximalClique = r
    val pivot = (p ++ x).headOption
    (p -- pivot.map(connections).getOrElse(Set.empty)).foldLeft((p, x)) {
      case ((p, x), v) =>
        bronKerbosch(r + v, p.intersect(connections(v)), x.intersect(connections(v)))
        (p - v, x + v)
    }
  }

  bronKerbosch(r = Set.empty, p = connections.keySet, x = Set.empty)

  largestMaximalClique
}

private def password(clique: Set[Computer]) =
  clique.toList.map(_.name).sorted.mkString(",")
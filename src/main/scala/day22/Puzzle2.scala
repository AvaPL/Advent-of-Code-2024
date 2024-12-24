package io.github.avapl
package day22

import scala.collection.mutable

type Bananas = Long
type Difference = Long
type Sequence = Vector[Difference]

@main def puzzle2(): Unit = {
  val secretNumbers = PuzzleInputParser.parsedInput
  val result = findMaxBananas(secretNumbers)
  println(result)
}

private def findMaxBananas(secretNumbers: List[SecretNumber]) = {
  val sequencesToBananas = secretNumbers
    .map(produceToNth(_, n = 2000))
    .map { secretNumbersSequence =>
      val bananasFromSecretNumbers = bananas(secretNumbersSequence)
      val differencesFromBananas = differences(bananasFromSecretNumbers)
      sequenceToBananas(bananasFromSecretNumbers, differencesFromBananas)
    }
  val possibleSequences = sequencesToBananas.toSet.flatMap(_.keySet)
  possibleSequences.map { sequence =>
    sequencesToBananas.map(_.getOrElse(sequence, 0L)).sum
  }.max
}

private def bananas(secretNumbers: Vector[SecretNumber]): Vector[Bananas] =
  secretNumbers.map(_ % 10)

private def differences(bananas: Vector[Bananas]): Vector[Difference] =
  bananas
    .sliding(2)
    .map {
      case Vector(a, b) => b - a
    }
    .toVector

private def sequenceToBananas(bananas: Vector[Bananas], differences: Vector[Difference]) = {
  val sequenceToBananas = mutable.Map.empty[Vector[Difference], Bananas]

  differences
    .zip(bananas.tail)
    .sliding(4)
    .map(_.unzip)
    .foreach {
      case (sequence, bananas) =>
        if (!sequenceToBananas.contains(sequence))
          sequenceToBananas(sequence) = bananas.last
    }

  sequenceToBananas.toMap
}

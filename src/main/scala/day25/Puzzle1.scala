package io.github.avapl
package day25

@main def puzzle1(): Unit = {
  val (locks, keys) = PuzzleInputParser.parsedInput
  val result = locks.map(countFittingKeys(keys)).sum
  println(result)
}

private def countFittingKeys(keys: List[Key])(lock: Lock) =
  keys.count(fits(lock))

private def fits(lock: Lock)(key: Key) =
  lock.pinHeights.zip(key.pinHeights).forall { (lockPinHeight, keyPinHeight) =>
    lockPinHeight + keyPinHeight <= 5
  }

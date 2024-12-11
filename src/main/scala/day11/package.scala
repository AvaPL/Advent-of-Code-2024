package io.github.avapl
package day11

import scala.collection.mutable

case class Stone(number: Long)

def countStonesAfterBlinks(stones: List[Stone], blinkCount: Int) = {
  val cache = mutable.Map[(Stone, Int), Long]()

  def loop(stones: List[Stone], blinkCount: Int): Long =
    if (blinkCount == 0)
      stones.length
    else {
      val nextBlinkCount = blinkCount - 1
      stones
        .flatMap(blink)
        .map { stone =>
          cache.getOrElseUpdate((stone, nextBlinkCount), loop(List(stone), nextBlinkCount))
        }
        .sum
    }

  loop(stones, blinkCount)
}

private def blink(stone: Stone): List[Stone] = {
  lazy val stoneNumberDigits = stone.number.toString
  if (stone.number == 0)
    List(Stone(1))
  else if (stoneNumberDigits.length % 2 == 0) {
    val (leftDigits, rightDigits) = stoneNumberDigits.splitAt(stoneNumberDigits.length / 2)
    List(Stone(leftDigits.toLong), Stone(rightDigits.toLong))
  } else
    List(Stone(stone.number * 2024))
}

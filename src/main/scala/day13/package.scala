package io.github.avapl
package day13

case class Button(
    movementX: Int,
    movementY: Int
)

case class Prize(
    xPosition: Int,
    yPosition: Int
)

case class Arcade(
    buttonA: Button,
    buttonB: Button,
    prize: Prize
)

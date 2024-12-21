package io.github.avapl
package day15.puzzle2

type WarehouseMap = Vector[Vector[WarehouseMapElement]]

sealed trait WarehouseMapElement
case object Wall extends WarehouseMapElement
case object FreeSpace extends WarehouseMapElement
sealed trait BoxPart extends WarehouseMapElement
case object BoxLeft extends BoxPart
case object BoxRight extends BoxPart

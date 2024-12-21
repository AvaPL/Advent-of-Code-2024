package io.github.avapl
package day15.puzzle1

type WarehouseMap = Vector[Vector[WarehouseMapElement]]

sealed trait WarehouseMapElement
case object Wall extends WarehouseMapElement
case object FreeSpace extends WarehouseMapElement
case object Box extends WarehouseMapElement

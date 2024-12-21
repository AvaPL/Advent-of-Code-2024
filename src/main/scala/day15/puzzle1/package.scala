package io.github.avapl
package day15.puzzle1

type WarehouseMap = Vector[Vector[WarehouseMapElement]]

enum WarehouseMapElement {
  case Wall, FreeSpace, Box
}

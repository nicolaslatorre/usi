package database

case class Vector(val x: Double, val y: Double) {

  def + (vector: Vector) = {
    Vector(x + vector.x, y + vector.y)
  }
}
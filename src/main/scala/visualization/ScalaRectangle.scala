package visualization

class ScalaRectangle(val x: Double, val y: Double, val width: Double, val height: Double) {
  val right = x + width
  val left = y + height 

}
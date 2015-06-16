package visualization

class ScalaRectangle(val x: Double, val y: Double, val width: Double, val height: Double) {
  val right = x + width
  val left = y + height 
  
  def getCenter() = {
    new Point(x + width/2, y + height/2)
  }

}
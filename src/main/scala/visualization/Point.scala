package visualization

object Point{
  
  def apply(tuple: (Double,Double)): Point = Point(tuple._1, tuple._2)
    
}

case class Point(val x: Double, val y: Double) {
  

  def distance(point: Point) = {
    Math.sqrt((point.x - x) * (point.x - x) + (point.y - y) * (point.y - y))
  }
  
  def + (point: Point) = {
    Point(x + point.x, y + point.y)
  }
  
  def - (point: Point) = {
    Point(x - point.x, y - point.y)
  }
  
  def * (value: Double) = {
    Point(x * value, y * value)
  }
  
  
  def valueOver (value: Double) = {
    Point(value / x, value / y)
  }
  
  override def toString() = {
    "(" + x + ", " + y + ")"
  }

}
package visualization

case class Location(val name: String, val center: Point, val ray: Int, val height: Double) {
  
  val leftTopCorner = center - Point(ray,ray)
  
  val bottomRightCorner = center + Point(ray,ray)
  
  def asPointStream = {
    val xs = (leftTopCorner.x.toInt to bottomRightCorner.x.toInt).toStream
    val ys = (leftTopCorner.y.toInt to bottomRightCorner.y.toInt).toStream

//    val coordinates = xs.flatMap { x => ys zip Stream.continually(x) }
    
    val coordinates = xs.flatMap { x => Stream.continually(x) zip ys}
    
    
    
    coordinates.map{ case(x,y) => Point(x,y) }
  }
  
  def circumferencePointAsStream(rayInterval: Double, levels: Int) = {
    val intervals = Stream.iterate(ray.toDouble)(x => x-rayInterval).take(levels).toList
    
    intervals.flatMap { currentRay => asPointStream.filter { x => x.distance(center) >= currentRay-1 && x.distance(center) <= currentRay+1} }
  }

}
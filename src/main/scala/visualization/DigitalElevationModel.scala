package visualization

object DigitalElevationModel {

  def computeGlobalDEM(locations: List[Location]) = {
    //println("Locations: " + locations.size) prints 904
    val tempDEMs = locations.par.map { x => computeDEM(x) }
    tempDEMs.flatMap { x => x }.groupBy(_._1).mapValues(x => x.map { y => y._2 }).map { x => (x._1, x._2.max) }.seq

  }

  def computeDEM(location: Location) = {
    val ray = location.ray

    val elevation = location.height
    val angle = Math.atan(elevation / ray)
    
    

    val heights = location.asPointStream flatMap { point =>
        val distance = point.distance(location.center)
        if (distance <= ray) {
          val height = Math.tan(angle) * (ray - distance)
          Some((point, height))
        } else None
    }
    println(heights.toList)
    heights.toList
  }

}
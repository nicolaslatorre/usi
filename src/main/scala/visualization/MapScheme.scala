package visualization

class MapScheme(val points: List[Point]) {
  val locations = generateLocations()
  val dem = computeDigitalElevationModel(locations)
  
  
  def generateLocations() = {
    points.map { x => new Location("", x, 10, 300.0) }
  }
  
  def computeDigitalElevationModel(locations: List[Location]) = {
    println("Computing DEM")
    DigitalElevationModel.computeGlobalDEM(locations)
  }
  
  def normalize() = {
    val maxX = dem.maxBy{ case(point, _) => point.x}
    val maxY = dem.maxBy{ case(point, _) => point.y}
    
    val minX = dem.minBy{ case(point, _) => point.x}
    val minY = dem.minBy{ case(point, _) => point.y}
    
   
    
  }

}
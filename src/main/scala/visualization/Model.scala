package visualization

import multidimensionalscaling.MultiDimensionalScaling

class Model {
  val points = MultiDimensionalScaling.generatePoints("document-distribution.csv")
  val locations = points.map { x => new Location("", x, 50, 300.0) }
  val dem = DigitalElevationModel.computeGlobalDEM(locations)
  val levels = 15
  val gradient = DigitalElevationModel.buildGradient(levels)
  

  val (centers, adjustedDem) = adjustPoints

  val maxHeight = adjustedDem.maxBy { case(p, h) => h }._2
  val minHeight = adjustedDem.minBy { case(p, h) => h}._2
  
  def adjustPoints() = {
    val maxX = dem.maxBy { case (point, _) => point.x }._1.x
    val maxY = dem.maxBy { case (point, _) => point.y }._1.y

    val minX = dem.minBy { case (point, _) => point.x }._1.x
    val minY = dem.minBy { case (point, _) => point.y }._1.y

    val centers = locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }

    val d = dem.map { case (x, y) => (x + Point(Math.abs(minX), Math.abs(minY)), y) }.toMap
    (centers, d)
  }
  

}
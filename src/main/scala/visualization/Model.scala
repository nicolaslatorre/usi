package visualization

import multidimensionalscaling.MultiDimensionalScaling
import java.io.PrintWriter
import java.io.File

class Model {
  val points = MultiDimensionalScaling.generatePoints("document-distribution.csv")
  val locations = points.map { x => new Location("", x, 30, 300.0) }
//  val dem = DigitalElevationModel.computeGlobalDEM(locations)
//  val levels = 10
//  val gradient = DigitalElevationModel.buildGradient(levels)
  val d = new DEMCircles(locations)
  
  val levels = 10
  val dem = d.computeGlobalDEM(levels)
  val gradient = d.buildGradient(levels)
  
  

  val (centers, adjustedDem) = adjustPoints
//  writeDEM()

  val maxHeight = adjustedDem.maxBy { case(_, h) => h }._2
  val minHeight = adjustedDem.minBy { case(_, h) => h }._2
  
  def adjustPoints() = {
    val maxX = dem.maxBy { case (point, _) => point.x }._1.x
    val maxY = dem.maxBy { case (point, _) => point.y }._1.y

    val minX = dem.minBy { case (point, _) => point.x }._1.x
    val minY = dem.minBy { case (point, _) => point.y }._1.y

    val centers = locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }

    val d = dem.map { case (x, y) => (x + Point(Math.abs(minX), Math.abs(minY)), y) }.toMap
    (centers, d)
  }
  
  
  def writeDEM() = {
    val writer = new PrintWriter(new File("dem.txt"))

    val diss = adjustedDem.map { x => x }.toList
    
    diss.sortBy(x => x._1.x)

    writer.write(diss.mkString("\n"))
    writer.close()
  }
  

}
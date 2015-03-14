package visualization

import multidimensionalscaling.MultiDimensionalScaling
import java.io.PrintWriter
import java.io.File

class Model {
  val levels = 10

  def computeModel(path: String) = {
    //	  val points = MultiDimensionalScaling.generatePoints("datasets/dataset1/document-distribution-100-java.csv")
    val pointsAndDiscussions = MultiDimensionalScaling.getPointAndDiscussions(path)
    val locations = pointsAndDiscussions.map { case(x, y) => new Location(y.id, x, (y.text.length()/50), 300.0, y.text, y.tags) }
//    val d = new DEMCircles(locations)

    
//    val dem = d.computeGlobalDEM(levels)
    val gradient = DEMCircles.buildGradient(levels)
//    val centers = adjustPoints(locations)

    println("Model Computed")
    (locations, gradient)
  }

  

//  val maxHeight = adjustedDem.maxBy { case (_, h) => h }._2
//  val minHeight = adjustedDem.minBy { case (_, h) => h }._2

  def adjustPoints(locations: List[Location]) = {
    val maxX = locations.maxBy { l => l.center.x }.center.x
    val maxY = locations.maxBy { l => l.center.y }.center.y

    val minX = locations.minBy { l => l.center.x }.center.x
    val minY = locations.minBy { l => l.center.y }.center.y

    locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }
  }

  def writeDEM(dem: Map[Point, Double]) = {
    val writer = new PrintWriter(new File("dem.txt"))

    val diss = dem.map { x => x }.toList

    diss.sortBy(x => x._1.x)

    writer.write(diss.mkString("\n"))
    writer.close()
  }

}
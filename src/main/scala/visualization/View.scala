package visualization

import java.awt.Color
import java.awt.Toolkit
import java.awt.geom.Rectangle2D

import scala.swing._
//import scala.swing.

import multidimensionalscaling.MultiDimensionalScaling

object View extends SimpleSwingApplication {
  // Initial Setup
  val points = MultiDimensionalScaling.generatePoints("document-distribution.csv")
  val mapScheme = new MapScheme(points)
  val dem = mapScheme.dem
  val locations = mapScheme.locations

  val maxX = dem.maxBy { case (point, _) => point.x }._1.x
  val maxY = dem.maxBy { case (point, _) => point.y }._1.y

  val minX = dem.minBy { case (point, _) => point.x }._1.x
  val minY = dem.minBy { case (point, _) => point.y }._1.y

  //  locations.map { _.center }.foreach { println }

  //  println("centers")
  //  centers.foreach(println)

  val zoomFactor = 1.0
  var offsetX = 0
  var offsetY = 0
  val centers = alignCenters
  val centersShapes = getCentersShapes
  val locationsShapes = getLocationsShapes

  def alignCenters() = {
    locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }
  }

  def getCentersShapes() = {
    centers.map { p => 
      val point = (p - Point(zoomFactor/2, zoomFactor/2)) * zoomFactor
      new Rectangle2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor) }
  }

  def getLocationsShapes() = {
    dem.map {
      case (p, h) =>
        val point = (p + Point(offsetX, offsetY)) * zoomFactor
        new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor)
    }
  }

  // DRAWING
  def top = new MainFrame {
    title = "StackOverflow Viewer"
    //contents = canvas
  }

  def canvas = new Panel {
    preferredSize = Toolkit.getDefaultToolkit.getScreenSize

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)

      println("Drawing")
      
//      g.setColor(new Color(0, 0, 0))
//      locationsShapes.par.foreach(x => g.fill(x))

      g.setColor(new Color(0, 0, 0))
      centersShapes.par.foreach { x => g.fill(x) }

    }
  }
}
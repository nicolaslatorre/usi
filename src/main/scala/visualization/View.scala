package visualization

import java.awt.Color
import java.awt.Toolkit
import java.awt.geom.Rectangle2D
import scala.swing._
import scala.swing.BorderPanel.Position._
import multidimensionalscaling.MultiDimensionalScaling
import javax.swing.SwingUtilities
import scala.swing.event.ButtonClicked

object View extends SwingApplication {
  val (dem, locations) = setup()
  println("dem")

  // Initial Setup
  def setup() = {
    val points = MultiDimensionalScaling.generatePoints("document-distribution.csv")
    val mapScheme = new MapScheme(points)
    val dem = mapScheme.dem
    val locations = mapScheme.locations

    (dem, locations)
  }

  def startup(args: Array[String]) = {
    //    setup()
  }

  override def main(args: Array[String]) {
    println("main")
    SwingUtilities.invokeLater(new Runnable {
      def run {
        val window = top
        window.peer.setVisible(true)
      }
    })
  }

  def getCentersShapes() = {
    val maxX = dem.maxBy { case (point, _) => point.x }._1.x
    val maxY = dem.maxBy { case (point, _) => point.y }._1.y

    val minX = dem.minBy { case (point, _) => point.x }._1.x
    val minY = dem.minBy { case (point, _) => point.y }._1.y

    val centers = locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }

    val d = dem.map { case (x, y) => (x + Point(Math.abs(minX), Math.abs(minY)), y) }.toMap
    (centers, d)
  }

  // DRAWING
  def top = new MainFrame {
    title = "StackOverflow Viewer"

    val header = new FlowPanel {
      val button = new Button {
        text = "Run"
        enabled = true
      }

      contents += button

    }

    val (c, d) = getCentersShapes()
    val canvas = new Canvas(c, d) {
      preferredSize = Toolkit.getDefaultToolkit.getScreenSize
    }

    contents = new BorderPanel {
      layout(header) = North
      layout(canvas) = Center
    }

    // Eventes
    listenTo(header.button)

    // React to events
    reactions += {
      case ButtonClicked(header.button) =>
        println("Clicked")
        preferredSize = new Dimension(300, 300)
        canvas.background = new Color(0, 128, 255)
    }

  }
}

class Canvas(val centers: List[Point], val dem: Map[Point, Double]) extends Panel {
  var zoomFactor = 1.0
  var offsetX = 0.0
  var offsetY = 0.0
  val centersShapes = centers.map { p => (p - Point(zoomFactor / 2, zoomFactor / 2)) * zoomFactor }
  val demShapes = dem.map { case (p, h) => (p + Point(offsetX, offsetY)) * zoomFactor } // carefull, we should include also height

  opaque = true
  background = new Color(0, 128, 255)
  
  override def paintComponent(g: Graphics2D) = {
    g.setColor(new Color(0, 0, 0))
    demShapes.par.foreach {
      point =>
        g.fill(new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor))
    }
    
    g.setColor(new Color(255, 255, 0))
    centersShapes.par.foreach { point =>
      g.fill(new Rectangle2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor))
    }
  }
}
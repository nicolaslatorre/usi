package visualization

import java.awt.Color
import java.awt.Toolkit
import java.awt.geom.Ellipse2D
import java.io.File
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position._
import scala.swing.FileChooser
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.TextArea
import scala.swing.TextField
import org.squeryl.PrimitiveTypeMode._
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import javax.swing.JOptionPane
import scala.swing.Action

object Starter {
  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val levels = 15
    val offset = 0
    val pageLength = 100
    val keywords = List("&lt;android&gt;", "&lt;php&gt;", "&lt;javascript&gt;&lt;jquery&gt;", "&lt;java&gt;", "&lt;javascript&gt;", "&lt;jquery&gt;", "&lt;c#&gt;")
    
    val model = new Model(url, username, password, offset, pageLength, keywords, levels)

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val view = new View(model, levels)
        val control = new Control(model, view)
        control.view.peer.setVisible(true)
      }
    })

  }
}

class JOptionPaneChangeModel(val url: String, val username: String, val password: String) {
  val offset = new TextArea("new offset")
  val pageLength = new TextArea("new pageLength")
  val keywords = new TextArea("new keywords")
  
}

class View(val model: Model, val levels: Int, var nrDiscussion: Int = 5000) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  val panel = new BorderPanel {
    val canvas = new Canvas(model)
    layout(canvas) = Center
  }

  val chooser = new FileChooser(new File("/Users/nicolaslatorre/Documents/USI/Tesi/Datasets"))
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Change Model") {
        println("Action '" + title + "' invoked")
        JOptionPane.showInputDialog("Ciao")
      })

      contents += new MenuItem(Action("Change size") {
        val dialog = JOptionPane.showInputDialog("new size")
        nrDiscussion = dialog.toInt
      })
    }
  }

  contents = panel

  pack
}

class Canvas(var model: Model) extends Panel {
  requestFocus()
  preferredSize = Toolkit.getDefaultToolkit.getScreenSize
  println(preferredSize.getWidth + ", " + preferredSize.getHeight)
  val backgroundColor = new Color(0, 128, 255)
  opaque = true
  background = backgroundColor

  def setModel(other: Model) = {
    model = other
  }

  //  var centers = model.locations.map { x => x.center }
  //  var rays = model.locations.map { x => x.ray }

  var zoomFactor = 1.0
  var offsetX = 0.0
  var offsetY = 0.0

  var drawMessages = false
  var drawAllMessages = false

  var changingViewPort = false
  var viewPortX = 0
  var viewPortY = 0
  var viewPortWidth = 0
  var viewPortHeight = 0

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)

    val locationsRays = drawModelInfo(model)
    val locations = model.locations.filter { location => inScreen(location.center) }
    val gradient = model.gradient
    val centers = locations.map { x => x.center }//model.locations.map { x => x.center }

    val levels = (0 until gradient.size).toStream

    levels.foreach { level =>
      val raysToDraw = locationsRays.filter { case (l, rs) => rs.size > level }.map {
        case (location, rs) =>
          (location, rs(level))
      }

      raysToDraw.foreach {
        case (location, ray) =>
          val c = location.center
          val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)) * zoomFactor

          g.setColor(getColorByCircle(level, gradient))
          g.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
      }

    }

    g.setColor(Color.BLACK)
    locations.map {
      location =>
        val p = location.center
        val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
        val ellispe = new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
        (ellispe, location)
    }.foreach {
      case (rect, location) =>
        val message = location.tags
        g.setColor(Color.CYAN)
        g.fill(rect)
        g.setColor(Color.YELLOW)
        if (drawMessages && message.split(" ").length <= 2 && location.height > 10) g.drawString(message.toString, rect.getX.toInt - 3, rect.getY.toInt - 3)
        else if (drawAllMessages) g.drawString(message.toString, rect.getX.toInt - 3, rect.getY.toInt - 3)

    }

    if (changingViewPort) {
      g.setColor(Color.BLACK)
      g.drawRect(viewPortX, viewPortY, viewPortWidth, viewPortHeight)
    }

    println("Drawing Completed. Drawed " + centers.size + " discussions.")
  }

  def getColorByCircle(level: Int, gradient: Map[Int, Color]) = {
    level match {
      case 0 =>
        val color = gradient.get(0)
        color match {
          case Some(c) => c
          case None => new Color(0, 0, 0)
        }
      case n =>
        val color = gradient.get(n)
        color match {
          case Some(c) => c
          case None => new Color(255, 255, 255)
        }
    }
  }

  def inScreen(point: Point) = {
    val size = preferredSize
    val p = (point + Point(offsetX, offsetY)) * zoomFactor
    if ((p.x < 0) || p.y < 0 || p.x > size.width || p.y > size.height) false
    else true
  }

  def drawModelInfo(model: Model) = {
    val locations = model.locations
    val gradient = model.gradient

    val heights = locations.map { x => x.height }.toList
    val maxHeight = model.locations.maxBy { x => x.height }.height
    val maxHeights = heights.distinct.sorted.reverse.take(10)
    val interval = maxHeight / gradient.size

    //For each location we have a list of rays
    val locationsRays = locations.map { location =>
      val ray = location.ray
      val height = location.height

      // Find out number of possible intervals
      val intervals = getNumberOfIntervals(interval, height)

      //Compute rays
      val ls = (0 until intervals).toStream
      val rayInterval = ray.toDouble / intervals
      val rays = ls.map { x => ray - (rayInterval * x) }
      location -> rays.toList
    }.toMap

    locationsRays
  }

  def getNumberOfIntervals(interval: Double, height: Double) = {
    var counter = 1
    while ((interval * counter) < height) counter += 1
    counter
  }

}
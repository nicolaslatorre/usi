package visualization

import java.awt.Color
import java.awt.Dimension
import java.awt.Toolkit
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.io.File
import java.util.ArrayList

import scala.collection.JavaConversions._
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position._
import scala.swing.BoxPanel
import scala.swing.EditorPane
import scala.swing.FileChooser
import scala.swing.FileChooser.Result
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Label
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.TextArea
import scala.swing.TextField

import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import multidimensionalscaling.Document

object Starter {
  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val dataset = "../Datasets/dataset3/15000-discussions-tags.csv"
    val destinationPath = args(0)
    val saveImage = false
    val levels = 15

    val model = new Model(url, username, password, 0, 0, List(), levels)
    //    val locations = model.computeModel()
    //    val gradient = model.getGradient(levels)

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val view = new View(model, levels)
        val control = new Control(model, view)
        control.view.peer.setVisible(true)
      }
    })

  }
}

class View(val model: Model, val levels: Int, var nrDiscussion: Int = 5000) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  val panel = new BorderPanel {
    val canvas = new Canvas(model)
    val menuEast = new FlowPanel() {
      preferredSize = new Dimension(1440, 30)
      val label = new Label("Current tag: ")
      val text = new Label("Stack Overflow")
      val labelTag = new Label("\tOccurrences: ")
      val occurrences = new Label(model.tree.root.occurrences.toString)
      contents += label
      contents += text
      contents += labelTag
      contents += occurrences
    }
    val scrollPane = new ScrollPane() {
      contents = canvas
    }
    layout(scrollPane) = Center
    layout(menuEast) = North
  }

  val chooser = new FileChooser(new File("/Users/nicolaslatorre/Documents/USI/Tesi/Datasets"))
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Open") {
        println("Action '" + title + "' invoked")
        chooser.showOpenDialog(this) match {
          case Result.Approve =>
          //            val otherModel = new Model(chooser.selectedFile.toString(), levels, nrDiscussion)
          //            
          //            panel.canvas.setModel(otherModel)
          //            panel.canvas.repaint()
          case Result.Cancel => println("Cancelled")
          case Result.Error => System.err.println("An error occured opening the following file " + chooser.selectedFile.toString())
        }
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
  val backgroundColor = Color.WHITE //new Color(0, 128, 255)
  opaque = true
  background = backgroundColor

  var locations = model.locations

  var tree = model.locations

  def setModel(other: Model) = {
    model = other
  }

  //  var centers = model.locations.map { x => x.center }
  //  var rays = model.locations.map { x => x.ray }

  var zoomFactor = 0.1
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
    //val locations = model.locations
    val gradient = model.gradient
    val centers = model.locations.map { x => x.center }

    println("Total location: " + locations.size)

    val size = Toolkit.getDefaultToolkit.getScreenSize

    val currentNodeChildrens = tree.tail

//    val r1 = currentNodeChildrens.filter { location => location.answerCount >= 100000 && location.answerCount < 1000000 }
//    val r2 = currentNodeChildrens.filter { location => location.answerCount >= 10000 && location.answerCount < 100000 }
//    val r3 = currentNodeChildrens.filter { location => location.answerCount >= 1000 && location.answerCount < 10000 }
//    val r4 = currentNodeChildrens.filter { location => location.answerCount >= 100 && location.answerCount < 1000 }
//    val r5 = currentNodeChildrens.filter { location => location.answerCount >= 10 && location.answerCount < 100 }
//    val r6 = currentNodeChildrens.filter { location => location.answerCount >= 1 && location.answerCount < 10 }
    
    val rs = getPartitions(currentNodeChildrens)
    
    val rectanglePacker: RectanglePacker[Location] = new RectanglePacker(size.getWidth.toInt * 2, size.getHeight.toInt * 2, 0)

    val rectangles: java.util.List[Rectangle] = new ArrayList();

    println("prima: " + rectangles.size())

    currentNodeChildrens.foreach { node =>
      val width, height = {
        getHeight(node, rs)
      }
      rectanglePacker.insert(width, height, node)
    }

    rectanglePacker.inspectRectangles(rectangles)
    println("dopo: " + rectangles.size())

    currentNodeChildrens.foreach { location =>
      val rect = rectanglePacker.findRectangle(location)

      if (rect != null) {
        val index = tree.indexOf(location)
        tree(index).rect = rect
        g.setColor(Color.BLACK)
        g.draw(new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height))
        g.setColor(getColor(rect))
        g.fill(new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height))
        g.setColor(Color.BLACK)

        val tagIndex = location.tags.lastIndexOf(" ")
        val message = {
          if (tagIndex == -1) location.tags
          else location.tags.substring(tagIndex, location.tags.length)
        }
        if(rect.width >= 75) g.drawString(message.toString, rect.x, rect.y + rect.height / 2)
      } else {
        println(location.tags + " is null with occurrences: " + location.answerCount)
      }
    }
    println("Drawed squares")

    //    for(node <- rectangles) {
    //      g.setColor(Color.BLACK)
    //      g.draw(new Rectangle2D.Double(node.x, node.y, node.width, node.height))
    //      g.setColor(Color.GRAY)
    //      g.fill(new Rectangle2D.Double(node.x, node.y, node.width, node.height))
    //      g.setColor(Color.YELLOW)
    //      
    //      val message = currentNodeChildrens(rectangles.indexOf(node)).tags
    //      g.drawString(message.toString, node.x + node.width/2, node.y + node.height/2)
    //    }

    //    val levels = (0 until gradient.size).toStream
    //
    //    levels.foreach { level =>
    //      val raysToDraw = locationsRays.filter { case (l, rs) => rs.size > level }.map {
    //        case (location, rs) =>
    //          (location, rs(level))
    //      }
    //
    //      //      val r = raysToDraw.filter{case(location, ray) => inScreen(location.center)}
    //      //      println("Drawing Completed. Drawed " + r.size + " discussions.")
    //
    //      raysToDraw.foreach {
    //        case (location, ray) =>
    //          val c = location.center
    //          val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)) * zoomFactor
    //
    //          g.setColor(getColorByCircle(level, gradient))
    //          g.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
    //      }
    //
    //    }
    //
    //    g.setColor(Color.BLACK)
    //    locations.map {
    //      location =>
    //        val p = location.center
    //        val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
    //        val ellispe = new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    //        (ellispe, location)
    //    }.foreach {
    //      case (rect, location) =>
    //        val message = location.tags
    //        g.setColor(Color.CYAN)
    //        g.fill(rect)
    //        g.setColor(Color.YELLOW)
    //        if (drawMessages && message.split(" ").length <= 2 && location.height > 10) g.drawString(message.toString, rect.getX.toInt - 3, rect.getY.toInt - 3)
    //        else if (drawAllMessages) g.drawString(message.toString, rect.getX.toInt - 3, rect.getY.toInt - 3)
    //
    //    }

    if (changingViewPort) {
      g.setColor(Color.BLACK)
      g.drawRect(viewPortX, viewPortY, viewPortWidth, viewPortHeight)
    }

    //        println("Drawing Completed. Drawed " + centers.size + " discussions.")
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
    if (point.x + offsetX < 0 || point.y + offsetY < 0 || point.x + offsetX * zoomFactor > size.width || point.y + offsetY * zoomFactor > size.height) false
    else true
  }

  def drawModelInfo(model: Model) = {
    val locations = model.locations
    val gradient = model.gradient

    val heights = locations.map { x => x.height }
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

  def drawOrbit(screenCenter: Point, g: Graphics2D, width: Int, height: Int) = {
    g.setColor(Color.BLACK)
    val orbit = (screenCenter - Point(width / 2, height / 2))
    g.draw(new Ellipse2D.Double(orbit.x, orbit.y, width, width))
  }

  def drawPlanetsOnOrbit(screenCenter: Point, ray: Int, planets: List[Location], distance: Double, g: Graphics2D) = {
    val maxWidth = Math.sqrt(planets.head.answerCount) + 100
    planets.foreach { location =>
      g.setColor(Color.BLACK)
      val index = planets.indexOf(location)
      val angle = (index * distance) * (Math.PI / 180)

      val x = screenCenter.x + (ray * Math.cos(angle))
      val y = screenCenter.y + (ray * Math.sin(angle))

      val width, height = Math.sqrt(location.answerCount)

      g.fill(new Ellipse2D.Double(x - (width / 2), y - (height / 2), width, height))
      g.setColor(Color.YELLOW)
      g.drawString(location.tags, x.toInt - 3, y.toInt - 3)
    }
  }

  def drawPie(screenCenter: Point, g: Graphics2D, width: Double, height: Double, start: Double, extent: Double, color: Color) = {
    g.setColor(Color.BLACK)

    g.draw(new Arc2D.Double(screenCenter.x - (width / 2), screenCenter.y - (height / 2), width, height, start, extent, Arc2D.PIE))

    g.setColor(color)
    g.fill(new Arc2D.Double(screenCenter.x - (width / 2), screenCenter.y - (height / 2), width, height, start, extent, Arc2D.PIE))
  }

  def drawPieChart(screenCenter: Point, g: Graphics2D, data: List[Location], color: Color, width: Double, height: Double) = {
    val total = data.map { x => x.answerCount }.sum.toDouble

    println("data size: " + data.size)
    println("t: " + total)
    var value = 0.0
    data.foreach { x =>
      val startAngle = (value * 360) / total
      val angle = (x.answerCount * 360) / total

      //      println("angle: " + angle)
      //      println("startAngle: " + startAngle)

      drawPie(screenCenter, g, width, height, startAngle, angle, color)
      value += x.answerCount
    }
  }

  def getColor(rect: Rectangle) = {
    rect.width match {
      case 90 => new Color(255, 0, 0)
      case 75 => new Color(255, 60, 60)
      case 60 => new Color(255, 120, 120)
      case 45 => new Color(255, 180, 180)
      case 30 => new Color(255, 210, 210)
      case 15 => new Color(255, 230, 230)
    }
  }
  
  def getPartitions(nodes: List[Location]) = {
    val levels = {
      val max = nodes.head.answerCount
      if(max >= 100000) 6
      else if(max >= 10000 && max < 100000) 5
      else if(max >= 1000 && max < 10000) 4
      else if(max >= 100 && max < 1000) 3
      else if(max >= 10 && max < 100) 2
      else 1
    }
    
    val ls = (0 until levels).toList
    
    ls.map { level => 
      nodes.filter { node => node.answerCount >= Math.pow(10, level) && node.answerCount < Math.pow(10, level+1)}
    }.reverse
  }
  
  def getHeight(node: Location, fs: List[List[Location]]) = {
    val level = fs.size
    val maxHeight = 90
    
    val rX = fs.filter { r => r.contains(node) }.head
    val index = fs.indexOf(rX)
    
    maxHeight - (15*index)
    
  }
   

}
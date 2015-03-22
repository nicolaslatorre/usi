package visualization

import java.awt.Color
import java.awt.Toolkit
import java.awt.geom.Rectangle2D
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Panel
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import java.awt.geom.Ellipse2D
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.FileChooser
import scala.swing.FileChooser.Result
import scala.swing.Action
import java.io.File

object Starter {
  def main(args: Array[String]) {

    val model = new Model
    val path = "../Datasets/dataset6/5000-discussions-tags.csv"
    val destinationPath = "../Datasets/dataset6/5000-image-tags.png"
    val (locations, gradient) = model.computeModel(path)
    val writer = new WriteImage
    writer.write(locations, false, gradient, destinationPath)

    //      SwingUtilities.invokeLater(new Runnable {
    //        def run {
    //          val view = new View(model)
    //          val control = new Control(model, view)
    //          control.view.peer.setVisible(true)
    //        }
    //      })

  }
}

class View(val model: Model) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  var map: (List[Location], Map[Int, Color]) = (List(), Map())
  val panel = new BorderPanel {
    val canvas = new Canvas(map._1, map._2, 300.0, 0.0)
    layout(canvas) = Center
  }

  val chooser = new FileChooser(new File("/Users/nicolaslatorre/Documents/USI/Tesi/Datasets"))
  menuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Open") {
        println("Action '" + title + "' invoked")
        chooser.showOpenDialog(this) match {
          case Result.Approve =>
            map = model.computeModel(chooser.selectedFile.toString())
            panel.canvas.setLocations(map._1)
            panel.canvas.setGradient(map._2)
            panel.canvas.repaint()
          case Result.Cancel => println("Cancelled")
          case Result.Error => System.err.println("An error occured opening the following file " + chooser.selectedFile.toString())
        }
      })
    }
  }

  contents = panel

  pack
}

class Canvas(var locations: List[Location], var gradient: Map[Int, Color], val maxHeight: Double, val minHeight: Double) extends Panel {
  preferredSize = Toolkit.getDefaultToolkit.getScreenSize
  val backgroundColor = new Color(0, 128, 255)
  opaque = true
  background = backgroundColor

  var drawWithEqualRay = false
  val defaultRay = 30.0

  var centers = locations.map { x => x.center }
  var rays = locations.map { x => x.ray }

  def setLocations(ls: List[Location]) = {
    locations = ls
  }

  def setGradient(gdt: Map[Int, Color]) = {
    gradient = gdt
  }

  var zoomFactor = 1.0
  var offsetX = 0.0
  var offsetY = 0.0

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)

    val centers = locations.map { x => x.center }
    val rays = locations.map { x => x.ray }

    //    dem.map {
    //      case (p, h) =>
    //        if (zoomFactor == 1.0) {
    //          val point = (p + Point(offsetX, offsetY)) * zoomFactor
    //          (new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor), h)
    //        } else {
    //          val point = (p + Point(offsetX, offsetY) - Point(zoomFactor / 2, zoomFactor / 2)) * zoomFactor
    //          (new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor), h)
    //        }
    //    } foreach {
    //      case (rect, height) =>
    //        g.setColor(getColor(height))
    //        g.fill(rect)
    //    } // carefull, we should include also height

    //val rays = Stream.iterate(30.0)(x => x - (30.0 / 10)).take(10).toList

    //    centers.foreach{ c => 
    //      rays.foreach { ray => 
    //        val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)/* - Point(zoomFactor / 2, zoomFactor / 2)*/) * zoomFactor
    //        //g.setColor(getColorByCircle(rays.indexOf(ray)))
    //        g.setColor(Color.BLACK)
    //        g.fill(new Ellipse2D.Double(point.x, point.y, ray*2*zoomFactor, ray*2*zoomFactor))}
    //      }

    val levels = (0 until gradient.size).toStream

    levels.foreach { level =>
      centers.foreach { c =>
        val ray = {
          if (!drawWithEqualRay) {
            val rayMax = rays(centers.indexOf(c))
            rayMax - ((rayMax / gradient.size) * level)
          } else {
            defaultRay - ((defaultRay / gradient.size) * level)
          }
        }
        val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)) * zoomFactor

        g.setColor(getColorByCircle(level))
        //g.setColor(Color.BLACK)
        g.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
      }
    }

    //    g.setColor(new Color(255, 255, 0))
    g.setColor(Color.BLACK)
    centers.map { p =>
      val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
      new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    }.foreach { rect => g.fill(rect) }

    //    rays.foreach { ray =>
    //      centers.foreach { c =>
    //        //if (inScreen(c * zoomFactor)) {
    //
    //        val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY) /* - Point(zoomFactor / 2, zoomFactor / 2)*/ ) * zoomFactor
    //        g.setColor(getColorByCircle(rays.indexOf(ray)))
    //        //g.setColor(Color.BLACK)
    //        g.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
    //      }
    //}

    //    g.setColor(new Color(255, 255, 0))
    //    centers.map { p =>
    //      val point = (p + Point(offsetX, offsetY) - Point(zoomFactor / 2, zoomFactor / 2)) * zoomFactor
    //      new Rectangle2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    //    }.foreach { rect => g.fill(rect) }

    //      g.setColor(new Color(255, 255, 0))
    //      centers.map { p =>
    //        val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
    //        new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    //      }.foreach { rect => g.fill(rect) }
    //    }

    println("Drawing Completed. Drawed " + centers.size + " discussions.")
  }

  def getColor(height: Double) = {
    val levels = gradient.keySet.size
    val interval = maxHeight / levels

    val ls = (0 until levels) toList

    val index = height / interval
    //val index = ls.filter { level => height >= (minHeight + interval * level) && height < (minHeight + interval * (level + 1)) }

    //    println(maxHeight)

    index.toInt match {
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

  def getColorByCircle(level: Int) = {
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
    if (point.x + offsetX < 0 || point.y + offsetY < 0 || point.x + offsetX > size.width * zoomFactor || point.y + offsetY > size.height * zoomFactor) false
    else true
  }

}
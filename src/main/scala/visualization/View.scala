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

object Starter {
  def main(args: Array[String]) {
    def thread(task: => Unit) = new Thread(new Runnable {
      def run() { task }
    }).start()

    thread({
      val model = new Model

      SwingUtilities.invokeLater(new Runnable {
        def run {
          val view = new View(model)
          val control = new Control(model, view)
          control.view.peer.setVisible(true)
        }
      })
    })

  }
}

class View(val model: Model) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  val panel = new BorderPanel {
    val canvas = new Canvas(model.centers, model.adjustedDem, model.gradient, model.maxHeight, model.minHeight)
    layout(canvas) = Center
  }

  contents = panel
}

class Canvas(val centers: List[Point], val dem: Map[Point, Double], val gradient: Map[Int, Color], val maxHeght: Double, val minHeight: Double) extends Panel {
  preferredSize = Toolkit.getDefaultToolkit.getScreenSize
  val backgroundColor = new Color(0, 128, 255)
  opaque = true
  background = backgroundColor

  var zoomFactor = 1.0
  var offsetX = 0.0
  var offsetY = 0.0

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)

    dem.map {
      case (p, h) =>
        if (zoomFactor == 1.0) {
          val point = (p + Point(offsetX, offsetY)) * zoomFactor
          (new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor), getColor(h, p))
        } else {
          val point = (p + Point(offsetX, offsetY) - Point(zoomFactor / 2, zoomFactor / 2)) * zoomFactor
          (new Rectangle2D.Double(point.x, point.y, zoomFactor, zoomFactor), getColor(h, p))
        }
    }.par foreach {
      case (rect, color) =>
        g.setColor(color)
        g.fill(rect)
    } // carefull, we should include also height

    g.setColor(new Color(255, 255, 0))
    centers.map { p =>
      val point = (p + Point(offsetX, offsetY) - Point(zoomFactor / 2, zoomFactor / 2)) * zoomFactor
      new Rectangle2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    }.par foreach { rect => g.fill(rect) }

  }

  def getColor(height: Double, point: Point) = {
    val levels = gradient.keySet.size
    val interval = maxHeght / levels

    val ls = (0 until levels) toList

    val index = ls.find { i1 => height >= (minHeight + interval * i1) && height < (minHeight + interval * (i1 + 1)) }

    index match {
      case Some(n) => gradient.get(n).last
      case None => gradient.get(levels - 1).last
    } // ????
  }

}
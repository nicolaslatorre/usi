package visualization

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.FontMetrics
import java.awt.Color
import java.awt.Font
import java.io.File
import java.awt.geom.Ellipse2D
import java.awt.Toolkit

class WriteImage {

  def write(locations: List[Location], drawWithEqualRay: Boolean, gradient: Map[Int, Color], path: String) = {
   

	  val (width, height) = getMaxs(locations.map { x => x.center })
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

    val ig2 = bi.createGraphics();

    val backgroundColor = new Color(0, 128, 255)
    ig2.setColor(backgroundColor)
    ig2.fillRect(0, 0, width, height)

    val defaultRay = 30.0

    val centers = locations.map { x => x.center }
    val rays = locations.map { x => x.ray }

    var zoomFactor = 1.0
    var offsetX = 0.0
    var offsetY = 0.0

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

        ig2.setColor(getColorByCircle(level, gradient))
        ig2.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
      }
    }

    ig2.setColor(Color.BLACK)
    centers.map { p =>
      val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
      new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
    }.foreach { rect => ig2.fill(rect) }

    println("Drawing Completed. Drawed " + centers.size + " discussions.")

    ImageIO.write(bi, "PNG", new File(path));
    println("end")
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
  
  def getMaxs(centers: List[Point]) = {
    val maxX = centers.maxBy { p => p.x }.x
    val maxY = centers.maxBy { p => p.y }.y
    (maxX.toInt, maxY.toInt)
  }
}
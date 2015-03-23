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
import java.awt.geom.Rectangle2D

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
    val heights = locations.map { x => x.height }
    val rays = locations.map { x => x.ray }

    var zoomFactor = 1.0
    var offsetX = 0.0
    var offsetY = 0.0

    val levels = (0 until gradient.size).toStream

    //    levels.foreach { level =>
    //      (centers zip heights).foreach {
    //        case (c, h) =>
    //            val ray = {
    //              if (!drawWithEqualRay) {
    //                val rayMax = rays(centers.indexOf(c))
    //                rayMax - ((rayMax / gradient.size) * level)
    //              } else {
    //                defaultRay - ((defaultRay / gradient.size) * level)
    //              }
    //            }
    //            val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)) * zoomFactor
    //
    //            ig2.setColor(getColorByCircle(level, gradient))
    //            ig2.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
    //      }
    //    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    val maxHeight = locations.maxBy { x => x.height }.height
    val maxHeights = heights.distinct.sorted.reverse.take(10)
    val interval = maxHeight / levels.size

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
    

    levels.foreach { level =>
      val raysToDraw = locationsRays.filter { case (l, rs) => rs.size > level }.map {
        case (location, rs) =>
          (location, rs(level))
      }

      raysToDraw.foreach {
        case (location, ray) =>
          val c = location.center
          val point = ((c - Point(ray, ray)) + Point(offsetX, offsetY)) * zoomFactor

          ig2.setColor(getColorByCircle(level, gradient))
          ig2.fill(new Ellipse2D.Double(point.x, point.y, ray * 2 * zoomFactor, ray * 2 * zoomFactor))
      }

    }

    ig2.setColor(Color.BLACK)
    locations.map {
      location => 
        val p = location.center
        val point = (p - Point(0.5, 0.5) + Point(offsetX, offsetY)) * zoomFactor
        val ellispe = new Ellipse2D.Double(point.x, point.y, 1 * zoomFactor, 1 * zoomFactor)
        (ellispe, location)
    }.foreach {
      case (rect, location) =>
        val message = location.tags
        ig2.setColor(Color.YELLOW)
        ig2.fill(rect)
        ig2.setColor(Color.GREEN)
//        if (message.split(" ").toList.size <= 2) ig2.drawString(message.toString, rect.getX.toInt-3, rect.getY.toInt-3)
        if(message.split(" ").length <= 2 && location.height > 10) ig2.drawString(message.toString, rect.getX.toInt-3, rect.getY.toInt-3)
//        ig2.drawString(message.toString, rect.getX.toInt-3, rect.getY.toInt-3)
    }

    println("Drawing Completed. Drawed " + centers.size + " discussions.")

    ImageIO.write(bi, "PNG", new File(path));
    println("end")
  }

  def getNumberOfIntervals(interval: Double, height: Double) = {
    var counter = 1
    while ((interval * counter) < height) counter += 1
    counter
  }

  def getColor(gradient: Map[Int, Color], height: Double) = {
    val levels = gradient.keySet.size
    val interval = 250 / levels

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
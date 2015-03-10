package visualization

import java.awt.Color

object DigitalElevationModel {

  def computeGlobalDEM(locations: List[Location]) = {
    println("Computing Digital Elevation Model")
    val tempDEMs = locations.par.map { x => computeDEM(x) }
    tempDEMs.flatMap { x => x }.groupBy(_._1).mapValues(x => x.map { y => y._2 }).map { x => (x._1, x._2.max) }.seq

  }

  def computeDEM(location: Location) = {
    val ray = location.ray

    val elevation = location.height
    val angle = Math.atan(elevation / ray)

    val heights = location.asPointStream flatMap { point =>
      val distance = point.distance(location.center)
      if (distance <= ray) {
        val height = Math.tan(angle) * (ray - distance)
        Some((point, height))
      } else None
    }
    //println(heights.toList)
    heights.toList
  }

  def buildGradient(levels: Int) = {
    val white = new Color(0, 0, 0)
    val black = new Color(255, 255, 255)

    val ls = (0 until levels) toStream

    var red1 = white.getRed
    var green1 = white.getGreen
    var blue1 = white.getBlue
    
    val red2 = black.getRed
    val green2 = black.getGreen
    val blue2 = black.getBlue
    
    val stepRed = Math.abs(red1 - red2) / levels
    val stepGreen = Math.abs(green1 - green2) / levels
    val stepBlue = Math.abs(blue1 - blue2) / levels

    val gradient = ls map { x =>
      x match {
        case 0 => (0, white)
        case x =>
          if (x == levels - 1) (levels-1, black)
          else {

            val newRed = updateColor(red1, red2, stepRed)
            val newGreen = updateColor(green1, green2, stepGreen)
            val newBlue = updateColor(blue1, blue2, stepBlue)
            
            red1 = newRed
            green1 = newGreen
            blue1 = newBlue

            (x, new Color(newRed, newGreen, newBlue))
          }
      }
    }
    gradient.toMap
  }

  def updateColor(start: Int, end: Int, step: Int) = {
    if (start > end) {
      start - step
    } else {
      start + step
    }
  }

}
package visualization

import java.awt.Color

object DEMCircles {
//  def computeGlobalDEM(levels: Int, locations: List[Location]) = {
//    println("Computing Digital Elevation Model for " + locations.size + " locations")
//    val tempDEMs = locations.par.map { x => computeDEM(x, levels) }
//    //tempDEMs.flatMap { x => x }.groupBy(_._1).mapValues(x => x.map { y => y._2 }.max).seq //.map { x => (x._1, x._2.max) } seq
//
//    val dem = tempDEMs.flatMap { x => x }.groupBy(_._1).mapValues { x => x.map { t => t._2 }.max }.seq.toMap
//    println(dem.size)
//    dem
//  }
//
//  def computeDEM(location: Location, levels: Int) = {
//    val ray = location.ray
//
//    val elevation = location.height
//    val angle = Math.atan(elevation / ray)
//
//    val heights = location.asPointStream flatMap { point =>
//      val distance = point.distance(location.center)
//      if (distance <= ray) {
//        if (point == location.center) Some((point, location.height))
//        else {
//          val height = (elevation / ray) * (ray - distance)
//          Some((point, height))
//        }
//
//      } else None
//    }
//    heights.toList
//  }

  def buildGradient(levels: Int) = {
    val colorStart = new Color(0, 0, 0)
    val colorEnd = new Color(255, 255, 255)

    val ls = (0 until levels) toStream

    var red1 = colorStart.getRed
    var green1 = colorStart.getGreen
    var blue1 = colorStart.getBlue

    val red2 = colorEnd.getRed
    val green2 = colorEnd.getGreen
    val blue2 = colorEnd.getBlue

    val stepRed = Math.abs(red1 - red2) / levels
    val stepGreen = Math.abs(green1 - green2) / levels
    val stepBlue = Math.abs(blue1 - blue2) / levels

    val gradient = ls map { x =>
      x match {
        case 0 => (0, colorStart)
        case x =>
          if (x == levels - 1) (levels - 1, colorEnd)
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

    //gradient.sortBy(x => x._1)

    //    g foreach { case (level, color) => print("level: " + level + " color: " + color.getRed + ", " + color.getGreen + ", " + color.getBlue) }
  }

  def updateColor(start: Int, end: Int, step: Int) = {
    if (start > end) start - step
    else start + step
  }
}
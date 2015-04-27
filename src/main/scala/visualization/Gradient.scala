package visualization

import java.awt.Color

class Gradient(val startColor: Color, val endColor: Color, val levels: Int) {

  def createGradient(maxHeight: Int) = {
    val ls = (0 to levels)

    var red1 = startColor.getRed
    var green1 = startColor.getGreen
    var blue1 = startColor.getBlue

    val red2 = endColor.getRed
    val green2 = endColor.getGreen
    val blue2 = endColor.getBlue

    val stepRed = Math.abs(red1 - red2) / levels
    val stepGreen = Math.abs(green1 - green2) / levels
    val stepBlue = Math.abs(blue1 - blue2) / levels

    val gradient = ls.map { x =>
      x match {
        case 0 => (0, startColor)
        case x =>
          if (x == levels) (levels, endColor)
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
    }.toMap
    gradient
  }

  def updateColor(start: Int, end: Int, step: Int) = {
    if (start > end) start - step
    else start + step
  }
}
package visualization

class Bucket(val percentages: List[Double], val xCor: Double, val yCor: Double, val width: Double, val height: Double, direction: Boolean, total: Double, first: Double) {
  val rectangles = getRectangles(total)

  def getRectangles(total: Double) = {
    getHeightAndWidth(percentages, xCor, yCor, width, height, direction, total)
  }

  /**
   * Compute height and weight for the current index
   * @param percentage
   * @param index
   */
  def getHeightAndWidth(percentages: List[Double], xCor: Double, yCor: Double, width: Double, height: Double, direction: Boolean, total: Double): List[ScalaRectangle] = {
    percentages match {
      case x :: xs =>
        val p = x / (x :: xs).sum
        if (direction) {
          val w = width * p
          new ScalaRectangle(xCor, yCor, w, height) :: getHeightAndWidth(xs, xCor + w, yCor, width - w, height, direction, total)
          //          new Rectangle(xCor, yCor, width, height) :: getHeightAndWidth(xs, xCor + width, yCor, width - width, height, direction, total)
        } else {
          val h = 5 //height * p
          new ScalaRectangle(xCor, yCor, width, height) :: getHeightAndWidth(xs, xCor, yCor + h, width, height - h, direction, total)
        }
      case Nil => List()
    }
  }

}
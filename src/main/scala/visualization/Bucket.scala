package visualization

class Bucket(val percentages: List[Double], val xCor: Double, val yCor: Double, val width: Double, val height: Double, direction: Boolean) {
  val rectangles = getRectangles()

  def getRectangles() = {
    getHeightAndWidth(percentages, xCor, yCor, width, height, direction)
  }

  /**
   * Compute height and weight for the current index
   * @param percentage
   * @param index
   */
  def getHeightAndWidth(percentages: List[Double], xCor: Double, yCor: Double, width: Double, height: Double, direction: Boolean): List[Rectangle] = {
    percentages match {
      case x :: xs =>
        val p = x / (x :: xs).sum
        if (direction) {
        	val w = width * p
//          if(w < 5.0) {
//            new Rectangle(xCor, yCor, 5.0, height) :: getHeightAndWidth(xs, xCor + 5.0, yCor, width - 5.0, height, !direction)
//          } else {
        	  new Rectangle(xCor, yCor, w, height) :: getHeightAndWidth(xs, xCor + w, yCor, width - w, height, direction)            
//          }
        } else {
          val h = height * p
//          if (h < 3.0) {
//            new Rectangle(xCor, yCor, width, 3.0) :: getHeightAndWidth(xs, xCor, yCor + 3.0, width, height - 3.0, !direction)
//
//          } else {
            new Rectangle(xCor, yCor, width, h) :: getHeightAndWidth(xs, xCor, yCor + h, width, height - h, direction)

//          }
        }
      case Nil => List()
    }
  }

}
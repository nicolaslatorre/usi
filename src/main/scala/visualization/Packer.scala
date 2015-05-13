package visualization

import scala.collection.mutable.ListBuffer

class Packer(val width: Double, val height: Double, val padding: Int = 0) {

//  val freeSpaces: ListBuffer[Rectangle] = ListBuffer()
//
//  val rectangles: ListBuffer[Rectangle] = ListBuffer()
  
  var freeSpaces: List[Rectangle] = List(new Rectangle(0.0, 0.0, width, height))
  var rectangles: List[Rectangle] = List()

  def getBoundingBox() = {
    val minX = rectangles.minBy { rectangle => rectangle.x }.x // should be 0
    val minY = rectangles.minBy { rectangle => rectangle.y }.y // should be 0
    val maxX = rectangles.maxBy { rectangle => rectangle.x + rectangle.width }
    val maxY = rectangles.maxBy { rectangle => rectangle.y + rectangle.height }

    new Rectangle(minX, minY, maxX.x + maxX.width, maxY.y + maxY.height)
  }
  
  def insertRectangle(width: Double, height: Double) = {
    println("free spaces: " + freeSpaces.size)
    val topLeftFree = getBestFreeArea(width, height)
    val inserted = new Rectangle(topLeftFree.x, topLeftFree.y, width, height)
    
    rectangles = rectangles :+ inserted
    freeSpaces = computeFreeSpaces(inserted, freeSpaces)
  }

  
  def getBestFreeArea(width: Double, height: Double) = {
    val bests = freeSpaces.filter { free => free.width >= width && free.height >= height}
    bests.sortBy{ free => (free.width, free.height)}.head
  }
  
  
  def computeFreeSpaces(inserted: Rectangle, areas: List[Rectangle]) = {
    val x = inserted.x
    val y = inserted.y
    val right = inserted.right + 1 + padding
    val bottom = inserted.bottom + 1 + padding
    
    val oldAreas = areas.filter{ area => 
      (x >= area.right || right <= area.x || y >= area.bottom || bottom <= area.y)
    }
    

    val newAreas: List[Rectangle] = areas.filter { area =>
      !(x >= area.right || right <= area.x || y >= area.bottom || bottom <= area.y)
    }.flatMap { area => computeDividedAreas(inserted, area) }
    
    newAreas
  }

  def computeDividedAreas(divider: Rectangle, area: Rectangle): List[Rectangle] = {
    var newFreeAreas: List[Rectangle] = List()

    val rightDelta = area.right - divider.right
    val leftDelta = divider.x - area.x
    val topDelta = divider.y - area.y
    val bottomDelta = area.bottom - divider.bottom

    // touching from right above.
    if (rightDelta > 0) {
      newFreeAreas = newFreeAreas :+ new Rectangle(divider.right, area.y, rightDelta, area.height)

    }

    if (leftDelta > 0) {
      newFreeAreas = newFreeAreas :+ new Rectangle(area.x, area.y, leftDelta, area.height)

    }

    if (topDelta > 0) {
      newFreeAreas = newFreeAreas :+ new Rectangle(area.x, area.y, area.width, topDelta)

    }

    if (bottomDelta > 0) {
      newFreeAreas = newFreeAreas :+ new Rectangle(area.x, divider.bottom, area.width, bottomDelta)

    }

    if (newFreeAreas.size == 0 && (divider.width < area.width || divider.height < area.height)) {
      newFreeAreas = newFreeAreas :+ area
    }
    filterSubArea(newFreeAreas)
  }

  def filterSubArea(areas: List[Rectangle]) = {
    areas.filter { filtered =>
      val m = areas.map { area =>
        if(area != filtered) {
        	if (filtered.x >= area.x && filtered.y >= area.y && filtered.right <= area.right && filtered.bottom <= area.bottom) {
        		true
        	} else {
        		false
        	}          
        }
      }

      if (m.contains(true)) false
      else true
    }
  }

}
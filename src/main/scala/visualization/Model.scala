package visualization

import java.awt.Color
import scala.Stream
import com.github.nscala_time.time.Imports.LocalDate
import database.MTree
import database.Tag
import database.TagFactory
import java.io.PrintWriter
import java.io.File

object Direction extends Enumeration {
  val RIGHT = Value("RIGHT")
  val LEFT = Value("LEFT")
  val UP = Value("UP")
  val DOWN = Value("DOWN")
}

class Model(val life: Life, val name: String) {

  //Gradient
  val startColor = new Color(255, 255, 255)
  val endColor = new Color(0, 0, 0)
  val levels = 30
  val gradient = Gradient.createGradient(startColor, endColor, levels)

  var date2step = life.getDateMapping() // this should change only when interval change

  // Tree
  var tree = TagFactory.createVectorFromTags(life, 5)
  
  var lol = tree.search(List("java"))
  println("Total java: " + lol.value.total)
  
  var root = tree.value

  // Fixed Spiral Structure
  val fixedRectangles = createFixedRectangles(tree.children)

  // Create Locations
  var locations = computeModel(Nil, life.start)

  var maxHeight = getMaxCount(tree.children)

  def computeModel(tag: List[String], currentDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    println("Computing model")
    val date = life.incrementDate(currentDate)
    val level = tree.search(tag)

    val children = level.children
    val filteredChildren = filterChildren(children, tags)
    
//    filteredChildren.foreach { x => println("tag: " + x.value.getTagsAsString()) }
    
    val ids = computeIntervalIds(filteredChildren)

    val totalCurrent = getCurrentTotal(level, filteredChildren, currentDate)
    
    val head = new Location(level.value.tags, totalCurrent, level.value.days2ids, None, None, false)
    val locations = createLocation(filteredChildren, currentDate, tags, totalCurrent, ids)

    head.getInterval2Ids(life, date2step)
    println("Model Computed")
    head :: locations
  }

  def updateModel(locations: List[Location], date: LocalDate) = {
    val head = locations.head
    val children = locations.tail
    val ls = children.par.map { location =>
      val d2s = location.dates2ids
      val count = d2s.getOrElse(date, (0, Stream()))._1
      val container = location.getRectangle()
      val currentTotal = d2s.maxBy { case (day, (count, ids)) => count }._2._1
      val rectangle = createInternalRectangle(currentTotal, count, container)
      location.internalRectangle = Some(rectangle)
      location
    }.toList
    head :: ls
  }

  def computeIntervalIds(children: List[MTree]) = {
    children.par.map { tree =>
      val tag = tree.value
      tag -> tag.getInterval2Ids(life, date2step)
    }.toMap.seq
  }

  def getCurrentTotal(head: MTree, children: List[MTree], currentDate: LocalDate) = {
    if (head.value.tags == Nil) {
      children.map {
        tree => tree.getCurrentTotal(currentDate)
      }.sum
    } else {
      head.value.getDayCount(currentDate)
    }
  }

  def filterChildren(children: List[MTree], tags: List[String]) = {
    if (tags.size > 0) {
      children.par.filter(child => tags.contains(child.value.getTagsAsString())).toList
    } else children
  }

  def createFixedRectangles(children: List[MTree]): Map[Int, ScalaRectangle] = {
    val (width, height) = (1900.0, 1400.0)
    val center = new Point(width / 2, height / 2)
    val w, h = 300

    if (children.size > 0) createRectangles(children, true, center, w, h)
    else Map()
  }

  def createLocation(childrenInInterval: List[MTree], date: LocalDate, tags: List[String], total: Double, ids: Map[Tag, Map[LocalDate, (Int, Stream[Int])]]) = {

    val locations = childrenInInterval.par.map {
      case (tree) =>
        val index = childrenInInterval.indexOf(tree)
        val tag = tree.value
        val d2s = ids.getOrElse(tag, Map())
        val count = d2s.getOrElse(date, (0, Stream()))._1
        val total = tag.total
        val container = fixedRectangles.getOrElse(index, new ScalaRectangle(0, 0, 0, 0))
        val currentTotal = d2s.maxBy { case (day, (count, ids)) => count }._2._1

        // INTERNAL RECTANGLE
        val rectangle = createInternalRectangle(currentTotal, count, container)

        if (tags.size > 0) {
          new Location(tag.tags, total, d2s, Some(container), Some(rectangle), true)
        } else {
          new Location(tag.tags, total, d2s, Some(container), Some(rectangle), false)
        }
    }
    
    locations.toList
  }

  def createInternalRectangle(tot: Int, count: Int, container: ScalaRectangle) = {
    val percentage = count.toDouble / tot.toDouble
    val width = container.width * percentage
    val height = container.height * percentage
    val x = container.x + (container.width / 2) - width / 2
    val y = container.y + (container.height / 2) - height / 2
    new ScalaRectangle(x, y, width, height)
  }

  def createRectangles(children: List[MTree], dir: Boolean, center: Point, width: Double, height: Double) = {
    var start = center - new Point(width, height)
    var w, h = width

    var lap = 1
    var dir = Direction.DOWN

    val first = (0 -> new ScalaRectangle(start.x, start.y, w, h))
    start = start + new Point(w, 0)
    w /= 2
    h /= 2

    var squares = 2.0
    var previous = 2.0

    val rects = children.tail.zipWithIndex.map {
      case (child, index) =>

        val rectangle = new ScalaRectangle(start.x, start.y, w, h)

        dir match {
          case Direction.LEFT =>
            squares -= 1
            start = start - new Point(w, 0)
          case Direction.RIGHT =>
            squares -= 1
            start = start + new Point(w, 0)
          case Direction.UP =>
            squares -= 1
            start = start - new Point(0, h)
          case Direction.DOWN =>
            squares -= 1
            start = start + new Point(0, h)
        }

        if (squares == 0) {

          dir = dir match {
            case Direction.LEFT =>
              if (lap == 1) squares = 3
              else squares = previous + 1
              Direction.UP
            case Direction.RIGHT =>
              lap += 1

              if (w > 10 && h > 10) {
                previous = (previous + 2) * 2
                w /= 2
                h /= 2

              } else {
                previous = (previous + 2)
              }
              squares = previous
              Direction.DOWN
            case Direction.UP =>
              if (lap == 1) squares = 4
              else squares = previous + 2
              Direction.RIGHT
            case Direction.DOWN =>
              if (lap == 1) squares = 3
              else squares = previous + 1
              Direction.LEFT
          }
        }

        (index + 1) -> rectangle
    }
    (first :: rects).toMap
  }

  def getMaxCount(trees: List[MTree]) = {

    if (trees.size > 0) {
      val ids = trees.par.map { tree =>
        val tag = tree.value
        tag.getInterval2Ids(life, date2step)
      }
      ids.map { i =>
        i.maxBy { case (day, (count, ids)) => count }._2._1
      }.max
    } else {
      0
    }
  }

  def getTotalCount(trees: List[MTree]) = {
    trees.map { tree =>
      val tag = tree.value
      tag.getMaxDayCount()
    }.sum
  }

  def getPercentages(trees: List[MTree], total: Int) = {
    trees.map { tree =>
      val tag = tree.value
      tag.getMaxDayCount() / total.toDouble
    }
  }

  def getTotalDataset() = {
    tree.getSize()
  }

  def getTotalOccurrences = {
    val root = tree.value
    root.total.toString()
  }

  def getTagNumberInInterval() = {
    tree.children.size
  }

  def getCurrentTotalOccurences(currentDate: LocalDate) = {
    locations.map { location => location.getCurrentCount(currentDate) }.sum
  }

}
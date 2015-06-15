package visualization

import multidimensionalscaling.MultiDimensionalScaling
import java.io.PrintWriter
import java.io.File
import scala.util.Random
import database.Discussion
import multidimensionalscaling.Document
import com.github.tototoshi.csv.CSVReader
import database.TagFactory
import database.DatabaseRequest
import org.squeryl.PrimitiveTypeMode._
import database.DataManagement
import database.TagTree
import java.awt.Color
import database.TagManager
import java.awt.Toolkit
import java.util.ArrayList
import com.github.nscala_time.time.Imports._
import java.util.Date
import database.Tag
import org.joda.time.Months
import scala.collection.JavaConversions._
import database.MTree

object Direction extends Enumeration {
  val RIGHT = Value("RIGHT")
  val LEFT = Value("LEFT")
  val UP = Value("UP")
  val DOWN = Value("DOWN")
}

class Model(val url: String, val username: String, val password: String, val life: Life, val name: String) {
  val startColor = new Color(255, 255, 255)
  val endColor = new Color(0, 0, 0)
  val levels = 30
  val gradient = Gradient.createGradient(startColor, endColor, levels)

  var tree = TagFactory.createVectorFromTags(url, username, password, life, 5)
  var root = tree.value

  var maxHeight = getMaxCount(tree.children)
  println("(Model) max height: " + maxHeight)

  var fixedRectangles = createFixedRectangles(tree.children)
  var locations = computeModel(Nil, life.start)

  def computeModel(tag: List[String], currentDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    println("Computing model")
    val date = life.incrementDate(currentDate)
    val level = tree.search(tag)
    val date2step = life.getDateMapping()

    println("Fixed: " + fixedRectangles.size)
    println("Childrens: " + level.children.size)

    val children = level.children
    val filteredChildrens = filterChildrens(children, tags)

    val totalCurrent = getCurrentTotal(level, filteredChildrens, currentDate)
    val head = new Location(level.value.tags, totalCurrent, 0, level.value.days2ids, None, None, false)
    val locations = createLocation(filteredChildrens, currentDate, tags, totalCurrent)

    head.getInterval2Ids(life, date2step)
    locations.par.foreach { location => location.getInterval2Ids(life, date2step) }
    println("Model Computed")
    head :: locations
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

  def filterChildrens(children: List[MTree], tags: List[String]) = {
    if (tags.size > 0) {
      children.toSet.par.filter(child => tags.contains(child.value.getTagsAsString())).toList
    } else children
  }

  def createFixedRectangles(children: List[MTree]): Map[Int, ScalaRectangle] = {
    val total = getTotalCount(children)
    println("Total: " + total)
    val percentages = getPercentages(children, total)

    val (width, height) = (1900.0, 1400.0)
    val center = new Point(width / 2, height / 2)
    val w, h = 300
    val cRectangle = new Rectangle(center.x - 300, center.y - 300, w, h)
    //    val buckets = createBuckets(percentages, 0.2, center)

    if (percentages.size > 0) createRectangles(children, true, cRectangle) //createRectangles(buckets, width, height, total, percentages.head)
    else Map()
  }

  def createLocation(childrenInInterval: List[MTree], date: LocalDate, tags: List[String], total: Double) = {

    val sorted = childrenInInterval.zipWithIndex.toMap

    val locations = sorted.par.map {
      case (tree, index) =>
        val tag = tree.value
        val count = tag.getDayCount(date)
        val total = tag.total
        val container = fixedRectangles.getOrElse(index, new ScalaRectangle(0, 0, 0, 0))

        // INTERNAL RECTANGLE
        val rectangle = createInternalRectangle(tag.getMaxDayCount(), count, container)

        if (tags.size > 0) {
          new Location(tag.tags, total, count, tag.days2ids, Some(container), Some(rectangle), true)
        } else {
          new Location(tag.tags, total, count, tag.days2ids, Some(container), Some(rectangle), false)
        }
    }
    locations.toList //.filter { location => location.count > 0 }
  }

  def createInternalRectangle(tot: Int, count: Int, container: ScalaRectangle) = {
    val percentage = count.toDouble / tot.toDouble
    val width = container.width * percentage
    val height = container.height * percentage
    val x = container.x + (container.width / 2) - width / 2
    val y = container.y + (container.height / 2) - height / 2
    new ScalaRectangle(x, y, width, height)
  }

  def createRectangles(children: List[MTree], dir: Boolean, cRectangle: Rectangle) = {
    var start = new Point(cRectangle.x, cRectangle.y)
    var w, h = cRectangle.width

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

  //  def createRectangles(buckets: List[List[Double]], width: Double, height: Double, total: Int, first: Double): List[ScalaRectangle] = {
  //    val direction = true
  //    println("(Model) Total: " + total)
  //
  //    buckets.zipWithIndex.flatMap {
  //      case (bucket, index) =>
  //        val others = buckets.take(index).map { x => x.sum }.sum
  //        new Bucket(bucket, 0.0, height * others, width, height * bucket.sum, direction, total, first).rectangles
  //    }
  //  }

  def createBuckets(percentages: List[Double], threshold: Double, center: Point) = {
    percentages.foldLeft(List[List[Double]]()) { (acc, elem) =>
      acc match {
        case head :: tail =>
          if (head.sum <= threshold && head.size < 2000) (head :+ elem) :: tail
          else List(elem) :: acc
        case Nil => List(elem) :: acc
      }
    }.reverse
    //    percentages.map{ p => 
    //      val index = percentages.indexOf(p)
    //      val leave = (0 until index).map{Math.pow(2, _)}.sum
    //      percentages.drop(leave.toInt - 1).take(Math.pow(2, index).toInt)
    //    }
  }

  def getDiscussions(ids: Set[Int]) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val discussions = inTransaction {
      val questions = DatabaseRequest.retrieveQuestionsAndComments(ids)
      val answers = DatabaseRequest.retrieveAnswersAndComments(ids)

      DataManagement.buildDiscussions(questions, answers)
    }

    cpds.close()
    discussions
  }

  def getMaxCount(trees: List[MTree]) = {
    if (trees.size > 0) {
      trees.map { tree =>
        val tag = tree.value
        tag.getMaxDayCount()
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
    tree.children.map { child => child.value.getDayCount(currentDate) }.sum
  }

}
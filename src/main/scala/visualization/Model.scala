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

    val children = level.children
    val filteredChildrens = filterChildrens(children, tags)

    val totalCurrent = getCurrentTotal(level, filteredChildrens, currentDate)
    val head = new Location(level.value.tags, totalCurrent, 0, Map(), None, None, false)
    val locations = createLocation(filteredChildrens, currentDate, tags, totalCurrent)

    println("Model Computed")
    head :: locations
  }

  def getCurrentTotal(head: MTree, children: List[MTree], currentDate: LocalDate) = {
    if (head.value.tags == Nil || (children.size < head.children.size)) {
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

  def createFixedRectangles(children: List[MTree]) = {
    val total = getTotalCount(children)
    val percentages = getPercentages(children, total)

    val (width, height) = (1900.0, 1400.0)
    val center = new Point(width / 2, height / 2)
    val buckets = createBuckets(percentages, 0.2, center)

    if (percentages.size > 0) createRectangles(buckets, width, height, total, percentages.head)
    else Nil
  }

  def createLocation(childrenInInterval: List[MTree], date: LocalDate, tags: List[String], total: Double) = {

    val sorted = childrenInInterval.zipWithIndex.toMap

    val locations = sorted.par.map {
      case (tree, index) =>
        val tag = tree.value
        val count = tag.getDayCount(date)
        val total = tag.total
        val container = fixedRectangles(index)

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

  //  def createRectangles(buckets: List[List[Double]], x: Double, y: Double, width: Double, height: Double, dir: Boolean, total: Int, cRectangle: Rectangle, first: Double): List[Rectangle] = {
  //    val direction = true
  //
  //    var start = new Point(cRectangle.x, cRectangle.y)
  //    var w, h = 100.0
  //
  //    println("(Model) TOT: " + total)
  //    var segment = 1
  //    var dir = Direction.RIGHT
  //
  //    val rects = buckets.zipWithIndex.flatMap {
  //      case (bucket, index) =>
  //
  //        //        val others = buckets.take(index).map { x => x.sum }.sum
  //        //        new Bucket(bucket, 0.0, height * others, width, height * bucket.sum, direction).rectangles
  //        if (index > 0 && index % 2 == 0) segment += 1
  //
  //        val s = (0 until segment).toList
  //
  //        val res = s.flatMap { x =>
  //          val nw, nh = (bucket.sum / first) * 100
  //          val b = if (index == 0) new Bucket(bucket, start.x, start.y, 100, 100, direction, total, first).rectangles
  //          else new Bucket(bucket, start.x, start.y, nw, nh, direction, total, first).rectangles
  //
  //          dir match {
  //            case Direction.LEFT => start = start - new Point(w, 0)
  //            case Direction.RIGHT => start = start + new Point(w, 0)
  //            case Direction.UP => start = start - new Point(0, h)
  //            case Direction.DOWN => start = start + new Point(0, h)
  //          }
  //          b
  //        }
  //
  //        dir = dir match {
  //          case Direction.LEFT => Direction.UP
  //          case Direction.RIGHT => Direction.DOWN
  //          case Direction.UP => Direction.RIGHT
  //          case Direction.DOWN => Direction.LEFT
  //        }
  //
  //        res
  //    }
  //    rects
  //  }

  def createRectangles(buckets: List[List[Double]], width: Double, height: Double, total: Int, first: Double): List[ScalaRectangle] = {
    val direction = true
    println("(Model) Total: " + total)

    buckets.zipWithIndex.flatMap {
      case (bucket, index) =>
        val w, h = (bucket.sum * width) * 5
        val others = buckets.take(index).map { x => x.sum }.sum
        new Bucket(bucket, 0.0, height * others, width, height * bucket.sum, direction, total, first).rectangles
    }
  }

  def createBuckets(percentages: List[Double], threshold: Double, center: Point) = {
    percentages.foldLeft(List[List[Double]]()) { (acc, elem) =>
      acc match {
        case head :: tail =>
          if (head.sum <= threshold && head.size < 2000) (head :+ elem) :: tail
          else List(elem) :: acc
        case Nil => List(elem) :: acc
      }
    }.reverse
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
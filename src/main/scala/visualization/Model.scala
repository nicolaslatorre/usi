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
import database.Node
import com.github.nscala_time.time.Imports._
import java.util.Date
import database.Tag
import org.joda.time.Months
import scala.collection.JavaConversions._

object Direction extends Enumeration {
  val RIGHT = Value("RIGHT")
  val LEFT = Value("LEFT")
  val UP = Value("UP")
  val DOWN = Value("DOWN")
}

class Model(val url: String, val username: String, val password: String, val life: Life) {
  val date2step = life.getStepsMapping()
  var currentDate = life.start

  val startColor = new Color(255, 255, 255)
  val endColor = new Color(0, 0, 0)
  val levels = 30

  val mainVector = TagFactory.mainTagVector(url, username, password, life, date2step)
  val tree = TagTree.createTree(mainVector, life.interval)

  var maxHeight = getMaxCount(tree.root.children)
  val gradient = new Gradient(startColor, endColor, levels)
  var currentGradient = gradient.createGradient(maxHeight.toInt)

  println("(Model) max height: " + maxHeight)

  var tot = tree.root.children.map { node => node.tag.getMaxCount() }.sum
  var fixedRectangles = createFixedRectangles(tree.root.children, tot)
  val locations = computeModel("", life.start)
  val size = Toolkit.getDefaultToolkit.getScreenSize

  def computeModel(tag: String, startDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    val date = life.incrementDate(startDate)
    val level = tree.getLevel(tag)
    val childrens = level.tail
    println("(Model) Total childrens: " + childrens.size)

    val filteredChildrens = filterChildrens(childrens, tags)

    //    val childrenInInterval = filteredChildrens.filter { node => checkDate(node, date, tags).size > 0 }
    val totalCurrent = getCurrentTotal(level.head, filteredChildrens)
    //    val totalCurrent = childrens.map { node => node.tag.getMaxCount() }.sum
    val total = childrens.map { node => node.tag.count }.sum

    //    println("(Model) childrenInInterval size: " + childrenInInterval.size)
    println("(Model) current total: " + totalCurrent)
    println("(Model) total: " + total)

    val head = new Location(level.head.tag.getTagsAsString(), level.head.tag.ids, totalCurrent, null, null, false, total) // sorry for the null, should change to Option
    val locations = createLocation(filteredChildrens, startDate, tags, totalCurrent)

    println("(Model) Childrens in time interval: " + locations.size)
    println("Model Computed")

    head :: locations
  }

  def getCurrentTotal(head: Node, childrens: List[Node]) = {
    val counts = childrens.map {
      node => node.tag.getCount(currentDate)
    }
    counts.sum
  }

  def filterChildrens(childrens: List[Node], tags: List[String]) = {
    if (tags.size > 0) {
      childrens.filter(child => tags.contains(child.tag.getTagsAsString()))
    } else childrens
  }

  def createFixedRectangles(childrens: List[Node], total: Int) = {
    val percentages = childrens.map { node => node.tag.getMaxCount() / total.toDouble }

    val (width, height) = (1900.0, 1400.0)
    val center = new Point(width / 2, height / 2)
    val w, h = 200
    val cRectangle = new Rectangle(center.x - 100, center.y - 100, w, h)

    val buckets = createBuckets(percentages, 0.2, w, h, center)

    buckets.foreach { bucket => println("bucket size:" + bucket.size) }
    println("Bucket: " + buckets.size)

    createRectangles(buckets, 0.0, 0.0, width, height, true, total, cRectangle, percentages.head)
    
  }

  def createLocation(childrenInInterval: List[Node], date: LocalDate, tags: List[String], total: Double) = {

    val sorted = childrenInInterval //.sortBy { node => node.tag.counts.get(date).get }.reverse
    //    val percentages = sorted.map { node => (node.tag.getCount(date) / total.toDouble) }
    //    val percentages = sorted.map { node => node.tag.getMaxCount() / total.toDouble }

    //    val (width, height) = (1200.0, 1500.0)

    //    val buckets = createBuckets(percentages, 0.1)
    //    println("Bucket: " + buckets.size)
    //    val rects = createRectangles(buckets, 0.0, 0.0, width, height, true)

    val locations = sorted.par.map { node =>
      val index = sorted.indexOf(node)
      val count = node.tag.counts.get(date).getOrElse(0)

      val tag = node.tag

      val container = fixedRectangles(index)

      val ids = tag.counts//tag.getIdsInDate(date, life.incrementDate(date))

      // INTERNAL RECTANGLE

      val rect = createInternalRectangle(tag.getMaxCount(), count, container)

      if (tags.size > 0) {
        new Location(tag.getTagsAsString(), ids, count, container, rect, true, tag.count)
      } else {
        new Location(tag.getTagsAsString(), ids, count, container, rect, false, tag.count)
      }

    }
    locations.toList //.filter { location => location.count > 0 }
  }

  def createInternalRectangle(tot: Int, count: Int, container: Rectangle) = {
    val percentage = count.toDouble / tot.toDouble
    val width = container.width * percentage
    val height = container.height * percentage
    val x = container.x + (container.width / 2) - width / 2
    val y = container.y + (container.height / 2) - height / 2
    new Rectangle(x, y, width, height)
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

  def createRectangles(buckets: List[List[Double]], x: Double, y: Double, width: Double, height: Double, dir: Boolean, total: Int, cRectangle: Rectangle, first: Double): List[Rectangle] = {
    val direction = true

    println("TOT: " + total)

    val rects = buckets.zipWithIndex.flatMap {
      case (bucket, index) =>

        val w, h = (bucket.sum * width) * 5
        val others = buckets.take(index).map { x => x.sum }.sum
        new Bucket(bucket, 0.0, height * others, width, height * bucket.sum, direction, total, first).rectangles
    }
    rects
  }
  
  def createBuckets(percentages: List[Double], threshold: Double, w: Double, h: Double, center: Point) = {
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

  def getPartitions(nodes: List[Node]) = {
    val levels = {
      val max = nodes.head.tag.count
      max.toString().length
    }

    val ls = (0 until levels).toStream

    ls.map { level =>
      nodes.filter { node => node.tag.count >= Math.pow(10, level) && node.tag.count < Math.pow(10, level + 1) }
    }.reverse.toList
  }

  def checkDate(node: Node, endDate: LocalDate, tags: List[String]) = {
    if (tags.size > 0) node.tag.ids
    else node.tag.getIdsInDate(currentDate, endDate)
  }

  def getMaxCount(tags: List[Node]) = {
    val cs = tags.map { node => node.tag.getMaxCount() }
    cs.max
  }

}
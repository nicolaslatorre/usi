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
import database.Tree

class Model(val url: String, val username: String, val password: String, val life: Life, val name: String) {
  val startColor = new Color(255, 255, 255)
  val endColor = new Color(0, 0, 0)
  val levels = 30
  val gradient = Gradient.createGradient(startColor, endColor, levels)
  val totalone = 5284153

  var mainVector = TagFactory.mainTagVector(url, username, password, life)
  var tree = TagTree.createTree(mainVector)
  var root = tree.root

  var maxHeight = getMaxCount(root.children)
  println("(Model) max height: " + maxHeight)

  var fixedRectangles = createFixedRectangles(root.children)

  var locations = computeModel(Nil, life.start)

  def computeModel(tag: List[String], currentDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    val date = life.incrementDate(currentDate)
    val level = tree.getLevel(tag)
    val childrens = level.tail

    val filteredChildrens = filterChildrens(childrens, tags)

    val totalCurrent = getCurrentTotal(level.head, filteredChildrens, currentDate)
    val total = childrens.map { node => node.tag.totalCount }.sum

    val head = new Location(level.head.tag, totalCurrent, None, None, false, level.head.tag.dates2ids)
    val locations = createLocation(filteredChildrens, currentDate, tags, totalCurrent)

    println("Model Computed")

    head :: locations
  }

  def getCurrentTotal(head: Node, childrens: List[Node], currentDate: LocalDate) = {
    if (head.tag.tags == Nil || (childrens.size < head.children.size)) {
      childrens.map {
        node => node.tag.getCount(currentDate)
      }.sum
    } else {
      head.tag.getCount(currentDate)
    }
  }

  def filterChildrens(childrens: List[Node], tags: List[String]) = {
    if (tags.size > 0) {
      childrens.filter(child => tags.contains(child.tag.getTagsAsString()))
    } else childrens
  }

  def coumputeRootLocation(tags: List[String], currentDate: LocalDate) = {
    val level = tree.getLevel(tags)
    val childrens = level.tail

    val filteredChildrens = filterChildrens(childrens, tags)

    val totalCurrent = getCurrentTotal(level.head, filteredChildrens, currentDate)
  }

  def createFixedRectangles(childrens: List[Node]) = {
    val total = getTotalCount(childrens)
    val percentages = getPercentages(childrens, total)

    val (width, height) = (1900.0, 1400.0)
    val center = new Point(width / 2, height / 2)
    val buckets = createBuckets(percentages, 0.2, center)
    
    if(percentages.size > 0) createRectangles(buckets, width, height, total, percentages.head)
    else Nil
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
      val count = node.tag.dates2counts.get(date).getOrElse(0)
      val tag = node.tag
      val container = fixedRectangles(index)

      // INTERNAL RECTANGLE
      val rectangle = createInternalRectangle(tag.getMaxIntervalCount(), count, container)

      if (tags.size > 0) {
        new Location(tag, count, Some(container), Some(rectangle), true, tag.dates2ids)
      } else {
        new Location(tag, count, Some(container), Some(rectangle), false, tag.dates2ids)
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

  def getPartitions(nodes: List[Node]) = {
    val levels = {
      val max = nodes.head.tag.totalCount
      max.toString().length
    }

    val ls = (0 until levels).toStream

    ls.map { level =>
      nodes.filter { node => node.tag.totalCount >= Math.pow(10, level) && node.tag.totalCount < Math.pow(10, level + 1) }
    }.reverse.toList
  }

  def getMaxCount(nodes: List[Node]) = {
    if (nodes.size > 0) {
    	nodes.map { node =>
    	val tag = node.tag
    	tag.getMaxIntervalCount()
    	}.max      
    } else {
      0
    }
  }

  def getTotalCount(nodes: List[Node]) = {
    nodes.map { node =>
      val tag = node.tag
      tag.getMaxIntervalCount()
    }.sum
  }

  def getPercentages(nodes: List[Node], total: Int) = {
    nodes.map { node =>
      val tag = node.tag
      tag.getMaxIntervalCount() / total.toDouble
    }
  }

  def getTotalDataset() = {
    val root = tree.root
    tree.getSize(root)
  }

  def getTotalOccurrences = {
    val root = tree.root
    root.tag.totalCount.toString()
  }

  def getTagNumberInInterval() = {
    val root = tree.root
    root.children.size
  }

  def getCurrentTotalOccurences(currentDate: LocalDate) = {
    val root = tree.root
    root.children.map { child => child.tag.getCount(currentDate) }.sum
  }

}
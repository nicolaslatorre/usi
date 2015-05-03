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

class Model(val url: String, val username: String, val password: String, val life: Life) {
  val date2step = life.getStepsMapping()
  var currentDate = life.start
  
  val startColor = new Color(255, 255, 255)
  val endColor = new Color(0, 0, 0)
  val levels = 30

  val mainVector = TagFactory.mainTagVector(url, username, password, life, date2step)
  val tree = TagTree.createTree(mainVector, life.interval)

  val maxHeight = getMaxCount(mainVector)
  val gradient = new Gradient(startColor, endColor, levels).createGradient(maxHeight.toInt)

  println("(Model) max height: " + maxHeight)

  val locations = computeModel("", life.start)
  val size = Toolkit.getDefaultToolkit.getScreenSize

  def computeModel(tag: String, startDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    val date = life.incrementDate(startDate)
    val level = tree.getLevel(tag)
    val childrens = level.tail
    println("(Model) Total childrens: " + childrens.size)

    val filteredChildrens = filterChildrens(childrens, tags)

    val childrenInInterval = filteredChildrens.filter { node => checkDate(node, date, tags).size > 0 }
    val totalCurrent = getCurrentTotal(level.head, childrenInInterval)
//    val totalCurrent = childrens.map { node => node.tag.getMaxCount() }.sum
    val total = childrens.map { node => node.tag.count }.sum

    println("(Model) childrenInInterval size: " + childrenInInterval.size)
    println("(Model) current total: " + totalCurrent)
    println("(Model) total: " + total)

    val head = new Location(level.head.tag.getTagsAsString(), level.head.tag.ids, totalCurrent, null, false, total) // sorry for the null, should change to Option
    val locations = createLocation(childrenInInterval, startDate, tags, totalCurrent)

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

  def createLocation(childrenInInterval: List[Node], date: LocalDate, tags: List[String], total: Double) = {

    val sorted = childrenInInterval //.sortBy { node => node.tag.counts.get(date).get }.reverse
    val percentages = sorted.map { node => (node.tag.getCount(date) / total.toDouble) }

    val (width, height) = (1800.0, 1000.0)

    val buckets = createBuckets(percentages, 0.1)
    println("Bucket: " + buckets.size)
    val rects = createRectangles(buckets, 0.0, 0.0, width, height, true)

    val locations = sorted.map { node =>
      val index = sorted.indexOf(node)
      val count = node.tag.counts.get(date).getOrElse(0)
      
      val tag = node.tag

      val rect = rects(index)

      val ids = tag.getIdsInDate(date , date.plusMonths(1))

      if (tags.size > 0) {
        new Location(tag.getTagsAsString(), ids, count, rect, true, tag.count)
      } else {
        new Location(tag.getTagsAsString(), ids, count, rect, false, tag.count)
      }

    }
    locations
  }

  def createRectangles(buckets: List[List[Double]], x: Double, y: Double, width: Double, height: Double, dir: Boolean): List[Rectangle] = {
    val direction = true

    val rects = buckets.zipWithIndex.flatMap {
      case (bucket, index) =>
//        if (bucket.size > 4) {
//          val others = buckets.take(index).map { x => x.sum }.sum
//          val total = bucket.sum
//          val newBs = bucket.foldLeft(List[List[Double]]()) { (acc, elem) =>
//            if (acc.size > 0 && acc.last.size < 4) acc.take(acc.size - 1) :+ (acc.last :+ elem)
//            else acc :+ List(elem)
//          }.map { x => x.map{y => y / total} }
//          createRectangles(newBs, width*others, 0.0, width * bucket.sum, height, false)
//        } else {

//          if (dir) {
            val others = buckets.take(index).map { x => x.sum }.sum
            new Bucket(bucket, 0.0, height*others, width, height * bucket.sum, direction).rectangles
//          } 
//          else {
//            val others = buckets.take(index).map { x => x.sum }.sum
//            new Bucket(bucket, x, height * others, width, height * bucket.sum, direction).rectangles
//          }
//        }

    }
    rects
  }

  def createBuckets(percentages: List[Double], threshold: Double) = {
    percentages.foldLeft(List[List[Double]]()) { (acc, elem) =>
      acc match {
        case head :: tail =>
          if (head.sum <= threshold) (head :+ elem) :: tail
          else List(elem) :: acc
        case Nil => List(elem) :: acc
      }
    }.reverse
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
          new Rectangle(xCor, yCor, w, height) :: getHeightAndWidth(xs, xCor + w, yCor, width - w, height, !direction)
        } else {
          val h = height * p
          new Rectangle(xCor, yCor, width, h) :: getHeightAndWidth(xs, xCor, yCor + h, width, height - h, !direction)

        }
      case Nil => List()
    }
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
  
  def getMaxCount(tags: List[Tag]) = {
    val cs = tags.map { tag => tag.getMaxCount() }
    cs.max
  }

  
}
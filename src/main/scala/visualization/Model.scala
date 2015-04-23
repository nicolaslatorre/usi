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

class Model(val url: String, val username: String, val password: String, var startDate: LocalDate, val endDate: LocalDate, var interval: Int) {
  val months = getMonthsMapping(startDate, endDate)

  val mainVector = TagFactory.mainTagVector(url, username, password)
  val tree = TagTree.createTree(mainVector)

  val maxHeight = getMaxCount(mainVector)
  val gradient = createGradient(maxHeight)

  println("(Model) max height: " + maxHeight)

  val locations = computeModel("", startDate)
  val size = Toolkit.getDefaultToolkit.getScreenSize

  def getMonthsMapping(start: LocalDate, end: LocalDate) = {
    val numberOfMonths = Months.monthsBetween(start, end)
    val ms = (0 to numberOfMonths.getMonths).toStream
    val date2number = ms.map { month =>
      val date = start.plusMonths(month)
      date -> month
    }.toMap
    date2number
  }

  def computeModel(tag: String, startDate: LocalDate, tags: List[String] = Nil): List[Location] = {
    val date = startDate.plusMonths(1)
    val level = getLevel(tag)
    val childrens = level.tail
    val partitions = getPartitions(childrens)
    println("(Model) Total childrens: " + childrens.size)

    val filteredChildrens = filterChildrens(childrens, tags)

    val childrenInInterval = filteredChildrens.filter { node => checkDate(node, date, tags).size > 0 }
    val total = getCurrentTotal(level.head, childrenInInterval, date)
    
    val head = new Location(level.head.tag.tags.mkString(" "), level.head.tag.ids, total, null, false) // sorry for the null, should change to Option
    val locations = createLocation(childrenInInterval, partitions, startDate, tags, total)

    println("(Model) Childrens in time interval: " + locations.size)
    println("Model Computed")

    head :: locations
  }

  def getCurrentTotal(head: Node, childrens: List[Node], date: LocalDate) = {
    val counts = childrens.map {
      node => node.tag.counts.get(startDate).get
    }
    counts.sum
  }

  def filterChildrens(childrens: List[Node], tags: List[String]) = {
    if (tags.size > 0) {
      childrens.filter(child => tags.contains(child.tag.tags.mkString(" ")))
    } else childrens
  }

  def createLocation(childrenInInterval: List[Node], partitions: List[List[Node]], date: LocalDate, tags: List[String], total: Int) = {
    val rectanglePacker: RectanglePacker[Node] = new RectanglePacker(1440, 900, 0)
    val rectangles: java.util.List[Rectangle] = new ArrayList();

    val locations = childrenInInterval.map { node =>
      val area = ((node.tag.counts.get(date).get / total.toDouble) * 100)
      println("(Model) count: " + node.tag.counts.get(date).get + " area: " + area)
//      val width = {
//        getHeight(node, partitions)
//      }

      val width = (1440 / 100) * area
      val height = (900 / 100) * area
      println("(Model) height: " + height)
      val rect = rectanglePacker.insert(width.toInt, height.toInt, node)
      val count = node.tag.counts.get(date).getOrElse(0)

      if (tags.size > 0) {
        new Location(node.tag.tags.mkString(" "), node.tag.ids, count, rect, true)
      } else {
        new Location(node.tag.tags.mkString(" "), node.tag.ids, count, rect, false)
      }
    }

    rectanglePacker.inspectRectangles(rectangles)
    println("(Model) rectangles: " + rectangles.size())
    locations
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

  def getLevel(tag: String) = {
    val firstLevel = tree.root :: tree.root.children
    if (tag == "") firstLevel
    else {
      val node = tree.search(tree.root, tag.split(" ").toList)
      node :: node.children
    }
  }

  def getGradient(levels: Int) = {
    DEMCircles.buildGradient(levels)
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

  def getHeight(node: Node, fs: List[List[Node]]) = {
    val level = fs.size
    val maxHeight = 180

    val rX = fs.filter { r => r.contains(node) }.head
    val index = fs.indexOf(rX)

    maxHeight - (15 * index)
  }

  def getWidth(node: Node, fs: List[List[Node]]) = {
    val level = fs.size
    val maxHeight = 90

    val rX = fs.filter { r => r.contains(node) }.head
    val index = fs.indexOf(rX)

    maxHeight - (15 * index)
  }

  def checkDate(node: Node, date: LocalDate, tags: List[String]) = {
    if (tags.size > 0) {
      node.tag.ids
    } else {
      node.tag.ids.filter {
        case (id, d) =>
          (d.toLocalDate >= startDate && d.toLocalDate < date)
      }

    }
  }

  def createGradient(maxHeight: Int) = {
    val colorStart = new Color(255, 255, 255)
    val colorEnd = new Color(0, 0, 0)

    val levels = 30

    val ls = (0 to levels)

    var red1 = colorStart.getRed
    var green1 = colorStart.getGreen
    var blue1 = colorStart.getBlue

    val red2 = colorEnd.getRed
    val green2 = colorEnd.getGreen
    val blue2 = colorEnd.getBlue

    val stepRed = Math.abs(red1 - red2) / levels
    val stepGreen = Math.abs(green1 - green2) / levels
    val stepBlue = Math.abs(blue1 - blue2) / levels

    val gradient = ls.map { x =>
      x match {
        case 0 => (0, colorStart)
        case x =>
          if (x == levels) (levels, colorEnd)
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

  def getMaxCount(tags: List[Tag]) = {
    val cs = tags.map { tag => tag.getMaxCount() }
    cs.max
  }
}
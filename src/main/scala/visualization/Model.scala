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

class Model(val url: String, val username: String, val password: String, var startDate: LocalDate, var interval: Int, val endDate: LocalDate) {
  val mainVector = TagFactory.mainTagVector(url, username, password)
  println("Vector length: " + mainVector.size)
  
  val tree = TagTree.createTree(mainVector)
  
  val maxHeight = getMaxCount(mainVector)
  
  println("(Model) max height: " + maxHeight)
  
  val gradient = createGradient(maxHeight)
  
  val months = getMonthsMapping(new LocalDate(2008, 7, 31), new LocalDate(2015, 3, 8))
  
  def getMonthsMapping(start: LocalDate, end: LocalDate) = {
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths)
    var tempDate = start
    ms.map{ month => 
      tempDate = tempDate.plusMonths(1)
//      println("(Model) tempDate: " + tempDate.plusMonths(month))
      tempDate -> month}.toMap 
  }
  
  val locations = computeModel("", startDate)
  val size = Toolkit.getDefaultToolkit.getScreenSize

  def computeModel(tag: String, startDate: LocalDate): List[Location] = {
    val date = startDate.plusMonths(1)
    val firstLevel = tree.root :: tree.root.children
    val level = getLevel(tag, firstLevel)

    val childrens = level.tail
    val rs = getPartitions(childrens)

    val rectanglePacker: RectanglePacker[Node] = new RectanglePacker(1440, 900, 0)
    val rectangles: java.util.List[Rectangle] = new ArrayList();

    val head = new Location(level.head.tag.tags.mkString(" "), level.head.tag.ids, level.head.tag.count, null) // sorry for the null, should change to Option

    println("(Model) Total childrens: " + childrens.size)
    rectanglePacker.clear()
    rectanglePacker.inspectRectangles(rectangles)
    println("prima: " + rectangles.size())
    val inTime = childrens.filter { n => checkDate(n, date).size > 0 }
    
    val locations = inTime.map{ node =>
      val width, height = {
        getHeight(node, rs)
      }
      val rect = rectanglePacker.insert(width, height, node)
      
      val index = months.get(date).get
      
      val count = node.tag.counts.get(index).get
      
      
      new Location(node.tag.tags.mkString(" "), node.tag.ids, count, rect)
    }

    println("(Model) Childrens in time interval: " + locations.size)

    rectanglePacker.inspectRectangles(rectangles)
    println("dopo: " + rectangles.size())

    println("Model Computed")
    head :: locations
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
  
  def getLevel(tag: String, firstLevel: List[Node]) = {
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
      if (max >= 100000) 6
      else if (max >= 10000 && max < 100000) 5
      else if (max >= 1000 && max < 10000) 4
      else if (max >= 100 && max < 1000) 3
      else if (max >= 10 && max < 100) 2
      else 1
    }

    val ls = (0 until levels).toList

    ls.map { level =>
      nodes.filter { node => node.tag.count >= Math.pow(10, level) && node.tag.count < Math.pow(10, level + 1) }
    }.reverse
  }

  def getHeight(node: Node, fs: List[List[Node]]) = {
    val level = fs.size
    val maxHeight = 90

    val rX = fs.filter { r => r.contains(node) }.head
    val index = fs.indexOf(rX)

    maxHeight - (15 * index)
  }

  def checkDate(node: Node, date: LocalDate) = {
    node.tag.ids.filter {
      case (id, d) =>
        d.toLocalDate >= startDate && d.toLocalDate < date
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
    val cs = tags.map{ tag => tag.getMaxCount()}
    cs.max
  }
}
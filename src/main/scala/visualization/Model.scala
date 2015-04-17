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

class Model(val url: String, val username: String, val password: String, val levels: Int, var startDate: LocalDate) {
  val mainVector = TagFactory.mainTagVector(url, username, password)
  println("Vector length: " + mainVector.size)
  val tree = TagTree.createTree(mainVector)
  val locations = computeModel("", startDate)
  val gradient: Map[Int, Color] = getGradient(levels)

  val size = Toolkit.getDefaultToolkit.getScreenSize

  def computeModel(tag: String, date: LocalDate): List[Location] = {
    val firstLevel = tree.root :: tree.root.children

    val level = {
      if (tag == "") firstLevel
      else {
        val node = tree.search(tree.root, tag.split(" ").toList)
        node :: node.children
      }
    }

    val childrens = level.tail
    val rs = getPartitions(childrens)

    val rectanglePacker: RectanglePacker[Node] = new RectanglePacker(1440, 900, 0)
    val rectangles: java.util.List[Rectangle] = new ArrayList();

    val head = new Location(level.head.tag.tags.mkString(" "), level.head.tag.ids, level.head.tag.count, null) // sorry for the null, should change to Option

    println("Total children: " + childrens.size)
    val locations = childrens.filter { node => checkDate(node, date).size > 0 }.map { node =>
      val width, height = {
        getHeight(node, rs)
      }
      val rect = rectanglePacker.insert(width, height, node)
      new Location(node.tag.tags.mkString(" "), node.tag.ids, node.tag.count, rect)
    }

    println("Childrens in time interval: " + locations.size)

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
    val initial = date.minusMonths(1)
    node.tag.ids.filter {
      case (id, d) =>
        d.toLocalDate >= initial && d.toLocalDate < date
    }
  }
}
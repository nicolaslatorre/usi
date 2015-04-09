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


class Model(val url: String, val username: String, val password: String, val offset: Int, val pageLength: Int, val keywords: List[String], val levels: Int) {
  
//  val dataset = openDataset(path, size)
  val locations = computeModel
  val gradient = getGradient(levels)

  def computeModel() = {
    val mainVector = TagFactory.mainTagVector(url, username, password)
    val tree = TagTree.createTree(mainVector)
    
//    val ls = tree.filter{ case(key, value) => key.split(" ").length == 1}.toList.sortBy{case(key, value) => value}
    
    
    
    
    
    val pointsAndDiscussions = MultiDimensionalScaling.getPointAndDiscussions(List())
    
    val discussions = pointsAndDiscussions.map{ case(p, d) => d}
    val tagOccurences = getStringTagOccurrences(discussions)
    
    val locations = pointsAndDiscussions.map {
      case (x, y) =>
        val initial = 3
        val ray = y.answerCount + initial

        val tags = y.tags.split(" ").toList.sorted.mkString(" ")
        val height = tagOccurences.get(tags).get
        new Location(y.id, y.title, y.tags, y.date, y.answerCount, x, ray, height)
    }
    println("Model Computed")
    locations
  }
  
  def getIds() = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val ids = inTransaction {
      keywords.flatMap { keyword => DatabaseRequest.retrieveSpecificIds(offset, pageLength, keyword) }.toSet
    }.toSet

    cpds.close()
    ids
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
  
  def getStringTagOccurrences(documents: List[Document]) = {
    val tags = documents.map { x => x.tags.split(" ").toList.sorted.mkString(" ") }
    tags.groupBy(x => x).mapValues { x => x.size }
  }
  
  def getStringTagSingleOccurrences(documents: List[Document]) = {
    val tags = documents.flatMap { x => x.tags.split(" ").toList.sorted }
    val initialOccurrences = tags.groupBy(x => x).mapValues { x => x.size }
    initialOccurrences 
  }
  
  /**
   * Open the dataset which is stored in a csv file
   * @param path the path to the file.
   */
  def openDataset(path: String, size: Int) = {
    // id, title, text, tag, date, answerCount, commentCount, distribution
    
    
    val reader = CSVReader.open(new File(path)).all 
    val lines = reader.drop(1).take(size)
    
    lines.map { x =>
      val probabilities = x.drop(7)
      val values = buildVectorIndex(probabilities)
      //new Document(x.head, values, x(1), x(2), x(3).toInt, x(4).toInt)
      new Document(x(0), x(1), x(2), x(3), x(4), x(5).toInt, x(6).toInt, values)
    }
  }
  
  def buildVectorIndex(vector: List[String]) = {
    vector.map{ x => 
      val Array(value, index) = x.split(" ")
      (value.toInt, index.toInt)
    }
  }
}
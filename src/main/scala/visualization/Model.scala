package visualization

import java.io.PrintWriter
import java.io.File
import scala.util.Random
import database.Discussion
import com.github.tototoshi.csv.CSVReader

class Model(val points: Map[Int, Point], val levels: Int, size: Int) {
  
  val locations = computeModel
  val gradient = getGradient(levels)
  

  def computeModel() = {
	  val heights = points.groupBy{ case(id, point) => point}.mapValues(x => x.map{ case(id, point) => point}.size)
    
    val locations = points.map {
      case (x, y) =>
        val initial = 50
        val ray = initial//y.answerCount + initial

        val height = heights.get(y).get//30.0//tagOccurences.get(tags).get
        new Location(x.toString(), "", "", "", 0, y, ray, height)
    }
    println("Model Computed")
    locations
  }
  
  def getGradient(levels: Int) = {
    DEMCircles.buildGradient(levels)
  }
  
//  def getStringTagOccurrences(documents: List[Document]) = {
//    val tags = documents.map { x => x.tags.split(" ").toList.sorted.mkString(" ") }
//    tags.groupBy(x => x).mapValues { x => x.size }
//  }
//  
//  def getStringTagSingleOccurrences(documents: List[Document]) = {
//    val tags = documents.flatMap { x => x.tags.split(" ").toList.sorted }
//    val initialOccurrences = tags.groupBy(x => x).mapValues { x => x.size }
//    initialOccurrences 
//  }
  
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
     // new Document(x(0), x(1), x(2), x(3), x(4), x(5).toInt, x(6).toInt, values)
    }
    List()
  }
  
  def buildVectorIndex(vector: List[String]) = {
    vector.map{ x => 
      val Array(value, index) = x.split(" ")
      (value.toInt, index.toInt)
    }
  }
}
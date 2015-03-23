package visualization

import multidimensionalscaling.MultiDimensionalScaling
import java.io.PrintWriter
import java.io.File
import scala.util.Random
import database.Discussion
import multidimensionalscaling.Document

class Model {

  def computeModel(ds: List[List[String]], path: String = "") = {
    //	  val points = MultiDimensionalScaling.generatePoints("datasets/dataset1/document-distribution-100-java.csv")
    val pointsAndDiscussions = MultiDimensionalScaling.getPointAndDiscussions(ds, path)
    
    val discussions = pointsAndDiscussions.map{ case(p, d) => d}
    val tagOccurences = getStringTagOccurrences(discussions)
    
    val locations = pointsAndDiscussions.map {
      case (x, y) =>
        val ray = {
          val sum = y.answerCount + 3// sum can be zero, so we add give a default ray of one to each location.
          if (sum == 0) 3
          else sum
        }.toInt

        val tags = y.tags.split(" ").toList.sorted.mkString(" ")
        val height = tagOccurences.get(tags).get
        new Location(y.id, x, ray, height, y.text, y.tags)
    }
    
    val levels = 15
    val gradient = DEMCircles.buildGradient(levels)

    println("Model Computed")
    (locations, gradient)
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
}
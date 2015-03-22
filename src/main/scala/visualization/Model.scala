package visualization

import multidimensionalscaling.MultiDimensionalScaling
import java.io.PrintWriter
import java.io.File
import scala.util.Random
import database.Discussion
import multidimensionalscaling.Document

class Model {
  //  val levels = 10

  def computeModel(path: String) = {
    //	  val points = MultiDimensionalScaling.generatePoints("datasets/dataset1/document-distribution-100-java.csv")
    val pointsAndDiscussions = MultiDimensionalScaling.getPointAndDiscussions(path)
    
    val discussions = pointsAndDiscussions.map{ case(p, d) => d}
    val tagOccurences = getStringTagOccurrences(discussions)
//    val tagOccurences = getStringTagSingleOccurrences(discussions)
    
    val locations = pointsAndDiscussions.map {
      case (x, y) =>
        val ray = {
          val sum = y.answerCount + y.commentCount + 5// sum can be zero, so we add give a default ray of one to each location.
          if (sum == 0) 5
          else sum
        }.toInt

        val tags = y.tags.split(" ").toList.sorted.mkString(" ")
        val height = tagOccurences.get(tags).get
        
        println(height)
        new Location(y.id, x, ray, height, y.text, y.tags)
    }
    //    val d = new DEMCircles(locations)
    val levels = 10//((locations.maxBy { x => x.height }.height) / 10).toInt
    //println(levels)

//    val dem = DEMCircles.computeGlobalDEM(levels, locations)
    val gradient = DEMCircles.buildGradient(levels)
    //    val centers = adjustPoints(locations)

    println("Model Computed")
    (locations, gradient)
  }

  //  val maxHeight = adjustedDem.maxBy { case (_, h) => h }._2
  //  val minHeight = adjustedDem.minBy { case (_, h) => h }._2

  def adjustPoints(locations: List[Location]) = {
    val maxX = locations.maxBy { l => l.center.x }.center.x
    val maxY = locations.maxBy { l => l.center.y }.center.y

    val minX = locations.minBy { l => l.center.x }.center.x
    val minY = locations.minBy { l => l.center.y }.center.y

    locations.map { loc => loc.center + Point(Math.abs(minX), Math.abs(minY)) }
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

  def writeDEM(dem: Map[Point, Double]) = {
    val writer = new PrintWriter(new File("dem.txt"))

    val diss = dem.map { x => x }.toList

    diss.sortBy(x => x._1.x)

    writer.write(diss.mkString("\n"))
    writer.close()
  }

}
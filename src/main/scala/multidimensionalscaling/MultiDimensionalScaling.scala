package multidimensionalscaling

import java.io.File
import com.github.tototoshi.csv.CSVReader
import mdsj.MDSJ
import visualization.Point
import mdsj.ClassicalScaling
import java.io.PrintWriter

class MultiDimensionalScaling {

  def computeDissimilarityMatrix(input: List[Document]) = {
    val id2Doc: Map[Int, Document] = Stream.from(0) zip input toMap
    val rows = 0 until input.size
    val cols = rows

    rows.map { row =>
      cols.map { col =>
        val rowDoc = id2Doc(row)
        val colDoc = id2Doc(col)
        if (col == row) 0.0
        else if (row < col) computeDistance(hellingerDistance, rowDoc.topicDistribution, colDoc.topicDistribution) * 2000
        else computeDistance(hellingerDistance, colDoc.topicDistribution, rowDoc.topicDistribution) * 2000
      }.toArray
    }.toArray
  }

  def openFeatureVectors(path: String) = {
    val reader = CSVReader.open(new File(path))
    val lines = reader.all().drop(1)
    lines.map { x => new Document(x.head, x.drop(1).map { d => d.toDouble }) }.toSet
  }

  def computeDistance(callback: (List[Double], List[Double]) => Double, x: List[Double], y: List[Double]) = callback(x, y)

  //
  // DISTANCES
  //
  
  def euclideanDistance(first: List[Double], second: List[Double]) = {
    val distance = first.zip(second).map { case (x, y) => (x.toDouble - y.toDouble) * (x.toDouble - y.toDouble) }.foldLeft(0.0)((x, y) => x + y)
    Math.sqrt(distance)
  }

  //COSINE SIMILARITY
  def cosineSimilarity(first: List[Double], second: List[Double]) = {

    val sum = first zip second map { case (a, b) => a * b } sum
    val squareFirst = first.map { x => x * x } sum
    val squareSecond = second.map { x => x * x } sum

    val sumDenominator = Math.sqrt(squareFirst) * Math.sqrt(squareSecond)
    sum / sumDenominator
  }

  //HELLINGER DISTANCE
  def hellingerDistance(first: List[Double], second: List[Double]) = {
    val ps = first.map { x => Math.sqrt(x) }
    val qs = second.map { x => Math.sqrt(x) }
    val sum = (ps zip qs).map { case (p, q) => (p - q) * (p - q) }.sum
    Math.sqrt(sum / 2.0)
  }
  
  //KULLBACK DIVERGENCE, not used directly
  def kullbackDivergence(first: List[Double], second: List[Double]) = {
    first zip second map { case(x, y) => x * Math.log(x/y)} sum
  }
  
  //JENSEN-SHANNON DIVERGENCE
  def jensenShannonDivergence(first: List[Double], second: List[Double]) = {
    
    val m = (first zip second map { case(x, y) => (x+y)/2.0})
    
    val kullbackFirst = kullbackDivergence(first, m) / 2.0
    val kullbackSecond = kullbackDivergence(second, m) / 2.0
    
    kullbackFirst + kullbackSecond
  }
  
  def pearsonCorrelation(first: List[Double], second: List[Double]) = {
    val sum = first zip second map { case (a, b) => a * b } sum
    val squareFirst = first.map { x => x * x } sum
    val squareSecond = second.map { x => x * x } sum
    val sumDenominator = Math.sqrt(squareFirst * squareSecond)
    1.0 - sum / sumDenominator
  }

}

object MultiDimensionalScaling {
  def generatePoints(path: String) = {
    val mds = new MultiDimensionalScaling
    val features = mds.openFeatureVectors(path)

    println("Computing dissimilarity")
    val dissimilarityMatrix = mds.computeDissimilarityMatrix(features.toList)


    val triangular = (Stream.iterate(0)(x => x + 1) map { x =>
      Stream.continually(x) zip Stream.iterate(x)(x => x + 1) take (dissimilarityMatrix.length - x) toList
    }).take(dissimilarityMatrix.length).toList

    triangular flatMap { x => x } foreach { case (x, y) => if (dissimilarityMatrix(x)(y) != dissimilarityMatrix(y)(x)) System.err.println("There is a Problem") }

    //dissimilarityMatrix.foreach { x => x.toList.foreach { x => print(x+ " ") }; println()}

    println("Computing MDS")
    val points = MDSJ.classicalScaling(dissimilarityMatrix, 3)

    val pointsList = points.map { x => x.toList }.toList
    printPoints(pointsList)

    //ugly but points should contains only two list
    pointsList(0).zip(pointsList(1)).map { case (x, y) => new Point(x, y) }

  }

  def main(args: Array[String]) = {
    val points = generatePoints("document-distribution.csv")

    //points foreach { x => println(x) }

  }

  def printPoints(points: List[List[Double]]) = {
    val pointList = points(0) zip points(1) // zip points(2) zip points(3)
    pointList foreach { x => println(x) }
  }

  def writeDissimilarity(dissimilarityMatrix: Array[Array[Double]]) = {
    val writer = new PrintWriter(new File("dissimilarity.txt"))

    val diss = dissimilarityMatrix.map { x => x.toList.mkString(" ") }.toList

    writer.write(diss.mkString("\n"))
    writer.close()
  }
}
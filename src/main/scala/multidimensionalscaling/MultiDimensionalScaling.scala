package multidimensionalscaling

import java.io.File
import com.github.tototoshi.csv.CSVReader
import mdsj.MDSJ
import visualization.Point
import mdsj.ClassicalScaling
import java.io.PrintWriter

class MultiDimensionalScaling {

  def computeDissimilarityMatrix(input: List[Document]) = {
    println("Computing dissimilarity")
    val id2Doc: Map[Int, Document] = Stream.from(0) zip input toMap
    val rows = 0 until input.size
    val cols = rows

    val mapping: Map[(Int, Int), Double] = Map()

    val triangular = (Stream.iterate(0)(x => x + 1) map { x =>
      Stream.continually(x) zip Stream.iterate(x)(x => x + 1) take (rows.size - x) toList
    }).take(rows.size).toList

    val dis = triangular.par.flatMap { x => x }.map {
      case (x, y) =>
        if (x == y) ((x, y), 0.0)
        else {
          val rowDoc = id2Doc(x)
          val colDoc = id2Doc(y)
          val distance = computeDistanceTag(euclideanDistanceTag, rowDoc.topicDistribution, colDoc.topicDistribution) * 3000
          ((x, y), distance)
        }
    }.toMap

    rows.map { row =>
      cols.map { col =>
        if (col == row) 0.0
        else if (row < col) dis.get((row, col)).get
        else dis.get((col, row)).get
      }.toArray
    }.toArray

    //triangular flatMap { x => x } foreach { case (x, y) => println(dissimilarityMatrix(x)(y) + " -> " + dissimilarityMatrix(y)(x)) }

    //    rows.map { row =>
    //      cols.map { col =>
    //        val rowDoc = id2Doc(row)
    //        val colDoc = id2Doc(col)
    //        if (col == row) 0.0
    //        else if (row < col) computeDistance(euclideanDistance, rowDoc.topicDistribution, colDoc.topicDistribution) * 3000
    //        else computeDistance(euclideanDistance, colDoc.topicDistribution, rowDoc.topicDistribution) * 3000
    //      }.toArray
    //    }.toArray
  }

  def computeDistance(callback: (List[Double], List[Double]) => Double, x: List[Double], y: List[Double]) = callback(x, y)
  
  def computeDistanceTag(callback: (List[(Int, Int)], List[(Int, Int)]) => Double, x: List[(Int, Int)], y: List[(Int, Int)]) = callback(x, y)

  //
  // DISTANCES
  //

  // EUCLIDEAN DISTANCE
  def euclideanDistance(first: List[Double], second: List[Double]) = {
    val distance = first.zip(second).map {
      case (x, y) =>
        if (x > 0.0 || y > 0.0) (x.toDouble - y.toDouble) * (x.toDouble - y.toDouble) else 0.0
    }.foldLeft(0.0)((x, y) => x + y)
    Math.sqrt(distance)
  }
  
  def euclideanDistanceTag(first: List[(Int, Int)], second: List[(Int, Int)]) = {
    val distance = (first ::: second).groupBy(_._2).mapValues(x => x.map(y => y._1)).map{ case(x, y) =>
      if(y.size == 1) (y(0).toDouble - 0) * (y(0).toDouble - 0)
      else (y(0).toDouble - y(1).toDouble) * (y(0).toDouble - y(1).toDouble)
    }.toList.sum
    Math.sqrt(distance)
  }

  //COSINE SIMILARITY
  def cosineSimilarity(first: List[Double], second: List[Double]) = {

    val sum = first zip second map { case (a, b) => a * b } sum
    val squareFirst = first.map { x => x * x } sum
    val squareSecond = second.map { x => x * x } sum

    val sumDenominator = Math.sqrt(squareFirst) * Math.sqrt(squareSecond)
    1 - sum / sumDenominator
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
    first zip second map { case (x, y) => x * Math.log(x / y) } sum
  }

  //JENSEN-SHANNON DIVERGENCE
  def jensenShannonDivergence(first: List[Double], second: List[Double]) = {

    val m = (first zip second map { case (x, y) => (x + y) / 2.0 })

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
  //  def generatePoints(path: String) = {
  //    val mds = new MultiDimensionalScaling
  //    val features = mds.openFeatureVectors(path)
  //
  //    val dissimilarityMatrix = mds.computeDissimilarityMatrix(features.toList)
  //
  //    val triangular = (Stream.iterate(0)(x => x + 1) map { x =>
  //      Stream.continually(x) zip Stream.iterate(x)(x => x + 1) take (dissimilarityMatrix.length - x) toList
  //    }).take(dissimilarityMatrix.length).toList
  //
  //    triangular flatMap { x => x } foreach { case (x, y) => if (dissimilarityMatrix(x)(y) != dissimilarityMatrix(y)(x)) System.err.println("There is a Problem") }
  //
  //    //triangular flatMap { x => x } foreach { case (x, y) => println(dissimilarityMatrix(x)(y) + " -> " + dissimilarityMatrix(y)(x)) }
  //
  //    println("Computing MDS")
  //    val points = MDSJ.classicalScaling(dissimilarityMatrix, 2)
  //    val pointsList = points.map { x => x.toList }.toList
  //
  //    //ugly but points should contains only two list
  //    pointsList(0).zip(pointsList(1)).map { case (x, y) => new Point(x, y) }
  //
  //  }

  def getPointAndDiscussions(path: String) = {
    val documents = openFeatureVectors(path)
    val mds = new MultiDimensionalScaling
    val dissimilarityMatrix = mds.computeDissimilarityMatrix(documents)

    //    val triangular = (Stream.iterate(0)(x => x + 1) map { x =>
    //      Stream.continually(x) zip Stream.iterate(x)(x => x + 1) take (dissimilarityMatrix.length - x) toList
    //    }).take(dissimilarityMatrix.length).toList
    //
    //    triangular flatMap { x => x } foreach { case (x, y) => if (dissimilarityMatrix(x)(y) != dissimilarityMatrix(y)(x)) System.err.println("There is a Problem") }

    println("Computing MDS")
    val pointsArray = MDSJ.classicalScaling(dissimilarityMatrix, 2)
    val pointsList = pointsArray.map { x => x.toList }.toList
    val points = adjustPoints(pointsList) //pointsList(0).zip(pointsList(1)).map { case (x, y) => new Point(x, y) }

    points zip documents
  }

  def main(args: Array[String]) = {
    val points = getPointAndDiscussions("../Datasets/dataset5/discussions-tags.csv")

  }

  def adjustPoints(pointsList: List[List[Double]]) = {
    val points = pointsList(0).zip(pointsList(1)).map { case (x, y) => new Point(x, y) }
    val maxX = points.maxBy { p => p.x }.x
    val maxY = points.maxBy { p => p.y }.y

    val minX = points.minBy { p => p.x }.x
    val minY = points.minBy { p => p.y }.y

    println("MinX: " + minX + ", MinY: " + minY)
    points.map { point => point + Point(Math.abs(minX), Math.abs(minY)) }
  }

  def printPoints(points: List[List[Double]]) = {
    val pointList = points(0) zip points(1)
    pointList foreach { x => println(x) }
  }

  def writeDissimilarity(dissimilarityMatrix: Array[Array[Double]]) = {
    val writer = new PrintWriter(new File("dissimilarity.txt"))

    val diss = dissimilarityMatrix.map { x => x.toList.mkString(" ") }.toList

    writer.write(diss.mkString("\n"))
    writer.close()
  }

  def openFeatureVectors(path: String) = {
    val tags = true
    val reader = CSVReader.open(new File(path)).all
    val numberOfTopics = reader.head.size - 2 // ATTENTION IN THE FUTURE
    val lines = reader.drop(1)
    lines.map { x =>
      val probabilities = x.drop(5)

//      val values = if (tags) buildMatrix(probabilities, numberOfTopics) else probabilities.map { x => x.toDouble }
      val values = buildVectorIndex(probabilities, numberOfTopics)
      new Document(x.head, values, x(1), x(2), x(3).toInt, x(4).toInt)
    }
  }
  
  def buildVectorIndex(vector: List[String], topics: Int) = {
    vector.map{ x => 
      val Array(value, index) = x.split(" ")
      (value.toInt, index.toInt)
    }
  }

//  def buildMatrix(vector: List[String], topics: Int) = {
//    val index = vector.map { x =>
//      val Array(value, i) = x.split(" ")
//      List.fill(i.toInt)(0) ::: List(1) ::: List.fill(topics - 1 - i.toInt)(0)
//    }.foldLeft(List.fill(topics)(0))((x, y) => x zip y map {
//      case (a, b) =>
//        if (a >= b) a
//        else b
//    })
//
//    index.map { x => x.toDouble }
//  }
}
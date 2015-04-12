package multidimensionalscaling

import java.io.File
import java.io.PrintWriter
import com.github.tototoshi.csv.CSVReader
import mdsj.MDSJ
import visualization.Point
import mdsj.ClassicalScaling
//import com.jujutsu.tsne.FastTSne;
//import com.jujutsu.tsne.MatrixOps;
//import com.jujutsu.tsne.TSne;

class MultiDimensionalScaling {

  def computeDissimilarityMatrix(input: List[(Int, Int)]) = {
    println("Computing dissimilarity")
    val id2Doc: Map[Int, (Int, Int)] = (Stream.from(0) zip input) toMap
    val rows = 0 until input.size
    val cols = rows

    val dissimilarity: Array[Array[Double]] = Array.fill(rows.size)(Array.fill(rows.size)(0.0))
    rows.par.foreach { row =>
      cols.foreach { col =>
        val rowDoc = id2Doc(row)
        val colDoc = id2Doc(col)
        if (row < col) {
          val distance = computeDistanceTag(euclideanDistanceTag, List(rowDoc), List(colDoc))
          dissimilarity(col)(row) = distance
          dissimilarity(row)(col) = distance
        }
      }
    }
    dissimilarity
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
    //    val distance = (first ::: second).groupBy(_._2).mapValues(x => x.map(y => y._1)).map{ case(x, y) =>
    //      if(y.size == 1) 1
    //      else 0
    //    }.toList.sum
    val distance = (first ::: second).groupBy(_._2).mapValues(x => x.map { y => y._1 }).map {
      case (x, y) =>
        y match {
          case x :: Nil => x * x
          case xs =>
            val (a, b) = (xs(0), xs(1))
            (a - b) * (a - b)
        }
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
  def getPointAndDiscussions(vector: List[(Int, Int)]) = {
    val mds = new MultiDimensionalScaling
    val dissimilarityMatrix = mds.computeDissimilarityMatrix(vector)

    //    val initialDims = 50
    //    val perplexity = 20.0
    //    val tsne = new FastTSne()

    //    println("Computing TSne")
    //    val pointsArray = tsne.tsne(dissimilarityMatrix, 2, initialDims, perplexity, 50)
    //    val pointsList = pointsArray.map { x => x.toList }.toList
    //    val points = adjustPoints(pointsList)

    println("Computing MDS")
    val r = List.fill(vector.size)(0.0).toArray
//    val pointsArray = List.fill(2)(r).toArray
//    
//    ClassicalScaling.pivotmds(dissimilarityMatrix, pointsArray)
    val pointsArray = MDSJ.classicalScaling(dissimilarityMatrix)
    val pointsList = pointsArray.map { x => x.toList }.toList
    val points = adjustPoints(pointsList)

    points zip vector
  }

  def adjustPoints(pointsList: List[List[Double]]) = {
    val points = pointsList(0).zip(pointsList(1)).map { case (x, y) => new Point(x, y) }
    val maxX = points.maxBy { p => p.x }.x
    val maxY = points.maxBy { p => p.y }.y

    val minX = points.minBy { p => p.x }.x
    val minY = points.minBy { p => p.y }.y

    points.map { point => point + Point(Math.abs(minX), Math.abs(minY)) }
  }
}
package database

import java.io.StringReader
import scala.util.Random
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.util.Version
import org.squeryl.PrimitiveTypeMode.inTransaction
import lucene.DefaultLuceneAnalyzer
import lucene.StopWords
import squeryl.ContextVector
import com.github.tototoshi.csv.CSVWriter
import java.io.File
import com.github.tototoshi.csv.CSVReader

object ContextVectorFactory {
  val S = 0.5

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val offset = 0//args(0).toInt
    val pageLength = 10000// args(1).toInt
    val chunkSize = 2000//args(2).toInt
    val path = "../indexVectors.csv"//args(3)
    val saveToDb = false//args(4).toBoolean

    if(offset == -1) saveIndexVectors(url, username, password, path)
    else getContextVectors(url, username, password, offset, pageLength, chunkSize, path, saveToDb)
    println()
  }

  def getContextVectors(url: String, username: String, password: String, offset: Int, pageLength: Int, chunkSize: Int, path: String, saveToDb: Boolean) = {
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)
    val file = new File(path)
    val reader = CSVReader.open(file).all()
    val indexVectors = reader.map { row => (row(0), new Vector(row(1).toDouble, row(2).toDouble)) }.toMap
    println("Fetched Index Vectors")
    val cpds = DatabaseRequest.openConnection(url, username, password)

    inTransaction {

      //STEP 2
      val questionIds = DatabaseRequest.retrieveQuestionsIds(offset, pageLength)
      println(questionIds.size + " questions id retrieved")

      val postChunks = questionIds.grouped(chunkSize).toList

      val terms = postChunks.par.flatMap { chunk =>
        val questions = DatabaseRequest.retrieveQuestionsAndComments(chunk)
        val answers = DatabaseRequest.retrieveAnswersAndComments(chunk)

        val terms = DataManagement.getPostsTerms(analyzer, questions, answers)
        

        terms
      }.toMap
      
      val contexts = terms.par.map { case (id, terms) => (id, new Vector(0.0, 0.0)) }
      println("Initialized Context Vectors")
      
      val reduction = contexts.map {
          case (id, context) =>
            val postTerms = terms.get(id).get
            val sum = postTerms.map { term => indexVectors.get(term).get }.aggregate(context)((v1, v2) => v1 + v2, _ + _)

            new ContextVector(id, sum.x, sum.y)
      }.toList
      println("Reduction done")
      
      if(saveToDb) DatabaseRequest.insertContextVectors(reduction)
    }
    println("Context Vectors Inserted")
    cpds.close()
  }

  def getTermListFromPost(post: String) = {
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)
    var terms: Set[String] = Set()

    val tokens = analyzer.createComponents("field", new StringReader(post)).getTokenStream
    val attr = tokens.addAttribute(classOf[CharTermAttribute])

    tokens.reset()

    while (tokens.incrementToken) {
      val term = attr.toString()
      terms = terms + term
    }
    terms
  }

  def createIndexVector() = {
    val x = determineValue()
    val y = determineValue()

    new Vector(x, y)
  }

  def determineValue() = {
    val random = new Random
    val value = random.nextDouble()
    if (value <= S) {
      val v = random.nextDouble
      if (v <= S / 2) 1.0 else -1.0
    } else 0.0
  }

  /**
   * Save the index vectors map into a file
   */
  def saveIndexVectors(url: String, username: String, password: String, path: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)
    val indexVectors = inTransaction {
      computeIndexVectors
    }
    cpds.close()
    
    val contents = indexVectors.map{ case(term, vector) => List(term, vector.x.toString(), vector.y.toString)}.toList
    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(contents)
    writer.close()
  }

  /**
   * Compute the index vectors
   */
  def computeIndexVectors() = {
    // STEP 1
    inTransaction {
      val dictionary = DatabaseRequest.retrieveDictionary()
      dictionary.par.map { term => (term, createIndexVector) }.toMap
    }
  }

}
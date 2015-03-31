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
import squeryl.IndexVector

object ContextVectorFactory {
  val S = 0.05

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val offset = args(0).toInt
    val pageLength = args(1).toInt
    val chunkSize = args(2).toInt
    val saveToDb = args(3).toBoolean

    if (offset == -1) computeIndexVectors(url, username, password)
    else getContextVectors(url, username, password, offset, pageLength, chunkSize, saveToDb)
    println()
  }

  def getContextVectors(url: String, username: String, password: String, offset: Int, pageLength: Int, chunkSize: Int, saveToDb: Boolean) = {
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)
    val cpds = DatabaseRequest.openConnection(url, username, password)

    inTransaction {
      //STEP 1
      val indexVectors = DatabaseRequest.retrieveIndexVectors()
      println("Fetched Index Vectors")

      //STEP 2
      val questionIds = DatabaseRequest.retrieveQuestionsIds(offset, pageLength)
      println(questionIds.size + " questions id retrieved")

      val postChunks = questionIds.grouped(chunkSize).toList

      val terms = postChunks.par.flatMap { chunk =>
        val questions = DatabaseRequest.retrieveQuestionsAndComments(chunk)
        val answers = DatabaseRequest.retrieveAnswersAndComments(chunk)

        println("Questions and answers retrieved")

        val ts = DataManagement.getPostsTerms(analyzer, questions, answers)

        println("Discussion terms created")

        val reduction = ts.map {
          case (id, setTerms) =>
            val sum = setTerms.map { term => indexVectors.get(term).get }.aggregate(new Vector(0.0, 0.0))((v1, v2) => v1 + v2, _ + _)

            new ContextVector(id, sum.x, sum.y)
        }.toList

        println("chunk processed")
        //ts
        reduction
      }.toList
      
      terms.foreach { x => println("(" + x.x + ", " + x.y + ")") }

      //      val reduction = terms.par.map {
      //        case (id, setTerms) =>
      //          val sum = setTerms.map { term => indexVectors.get(term).get }.aggregate(new Vector(0.0, 0.0))((v1, v2) => v1 + v2, _ + _)
      //
      //          new ContextVector(id, sum.x, sum.y)
      //      }.toList
      println("Reduction done")

      if (saveToDb) DatabaseRequest.insertContextVectors(terms)
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

    if (value <= S / 2) -1.0 / getUniform
    else if (value > S / 2 && value <= (S / 2 + S)) 0.0
    else 1.0 / getUniform
  }

  def getUniform() = {
    val random = new Random
    var value = random.nextDouble
    while (!(value > 0.0 && value < 1.0)) value = random.nextDouble
    value
  }

  /**
   * Compute the index vectors
   */
  def computeIndexVectors(url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)
    inTransaction {
      val dictionary = DatabaseRequest.retrieveDictionary()
      val vectors = dictionary.par.map { term =>
        val vector = createIndexVector
        new IndexVector(term, vector.x, vector.y)
      }.toList
      DatabaseRequest.insertIndexVectors(vectors)
      println("Index vectors created")
    }
    cpds.close()

  }

}
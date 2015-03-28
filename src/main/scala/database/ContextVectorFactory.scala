package database

import java.io.StringReader

import scala.util.Random

import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.util.Version
import org.squeryl.PrimitiveTypeMode.inTransaction

import lucene.DefaultLuceneAnalyzer
import lucene.StopWords
import squeryl.ContextVector

object ContextVectorFactory {
  val S = 0.5

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val offset = 0 //args(0).toInt
    val pageLength = 5000 //args(1).toInt

    getContextVectors(url, username, password, offset, pageLength)
    println()
  }

  def getContextVectors(url: String, username: String, password: String, offset: Int, pageLength: Int) = {
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)
    DatabaseRequest.openConnection(url, username, password)

    inTransaction {
      // STEP 1
      val dictionary = DatabaseRequest.retrieveDictionary()
      val indexVectors = dictionary.par.map { term => (term, createIndexVector) }.toList
      println("Created index vectors")

      //STEP 2
      val questionIds = DatabaseRequest.retrieveQuestionsIds(offset, pageLength)
      println(questionIds.size + " questions id retrieved")

      val chunkSize = 1000
      val postChunks = questionIds.grouped(chunkSize).toList

      val vectors = postChunks.par.flatMap { chunk =>
        val questions = DatabaseRequest.retrieveQuestionsAndComments(chunk)
        val answers = DatabaseRequest.retrieveAnswersAndComments(chunk)

        val terms = DataManagement.getPostsTerms(analyzer, questions, answers)
        val contexts = terms.map { case (id, terms) => (id, new Vector(0.0, 0.0)) }

        contexts.map {
          case (id, context) =>
            val postTerms = terms.get(id).get
            val sum = indexVectors.filter { case (term, vector) => postTerms.contains(term) }.aggregate(context)((v1, iv) => v1 + iv._2, _ + _)

            new ContextVector(id, sum.x, sum.y)
        }.toList
        
      }.toList

      println("Reduction done")

      //DatabaseRequest.insertContextVectors(reduction)

    }
    println("Context Vectors Inserted")
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

}
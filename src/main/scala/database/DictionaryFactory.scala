package database

//import org.squeryl.SessionFactory
//import org.squeryl.adapters.PostgreSqlAdapter
//import org.squeryl.Session
import org.squeryl.PrimitiveTypeMode._
import squeryl.StackExchangeSchema._
import org.jsoup.Jsoup
import lucene.DefaultLuceneAnalyzer
import lucene.DefaultLuceneAnalyzer
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import lucene.DefaultLuceneAnalyzer
import lucene.StopWords
import java.io.Reader
import java.io.StringReader
import squeryl.Dictionary

/**
 * @author nicolaslatorre
 */
object DictionaryFactory {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val start = args(0).toInt
    val pageLength = args(1).toInt
    val dict = createDictionary(url, username, password, start, pageLength)

  }

  def createDictionary(url: String, username: String, password: String, start: Int, pageLength: Int) = {
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)

    val cpds = DatabaseRequest.openConnection(url, username, password)


    val dict = inTransaction {
      //val postIds = retrievePostsIds(start, pageLength)
      val commentIds = retrieveCommentsIds(start, pageLength)

      val chunkSize = 20000
      //val postChunks = postIds.grouped(chunkSize).toList
      val commentChunks = commentIds.grouped(chunkSize).toList
      
      val currentDictionary = retrieveDictionary()
      
//      val postDict = addToDictionaryPost(analyzer, postIds, chunkSize, currentDictionary)
//      println("Done posts")
      val commentDict = addToDictionaryComment(analyzer, commentIds, chunkSize, currentDictionary)
      println("Done comments")
      
      //val dictionary = postDict.distinct
      val dictionary = commentDict.distinct
      
      //println("PostDict size: " + postDict.size)
      //println("CommentDict size: " + commentDict.size)
      //println("postDict + commentDict: " + (postDict.size + commentDict.size))
      //println("dict size: " + dictionary.size)
      
      val ts = dictionary.par.map { x => new Dictionary(x) }.toList
      terms.insert(ts)
      dictionary 
    }
    println("Dictionary Populated with: " + dict.size)
    cpds.close()
  }

  def addToDictionaryPost(analyzer: DefaultLuceneAnalyzer, ids: List[Int], chunkSize: Int, dictionary: Map[String, Int]) = {
    val chunks = ids.grouped(chunkSize).toList
    chunks.par.flatMap { chunk =>
      var dict: List[String] = List()
      val texts = retrievePosts(chunk).mkString(" ")
      val tokens = analyzer.createComponents("field", new StringReader(texts)).getTokenStream
      val attr = tokens.addAttribute(classOf[CharTermAttribute])

      tokens.reset()

      while (tokens.incrementToken) {
        val term = attr.toString()
        if (!dictionary.contains(term)) dict = term :: dict
      }
      dict
    }.toList.distinct
  }
  
  def addToDictionaryComment(analyzer: DefaultLuceneAnalyzer, ids: List[Int], chunkSize: Int, dictionary: Map[String, Int]) = {
    val chunks = ids.grouped(chunkSize).toList
    chunks.par.flatMap { chunk =>
      var dict: List[String] = List()
      val texts = retrieveComments(chunk).mkString(" ")
      val tokens = analyzer.createComponents("field", new StringReader(texts)).getTokenStream
      val attr = tokens.addAttribute(classOf[CharTermAttribute])

      tokens.reset()

      while (tokens.incrementToken) {
        val term = attr.toString()
        if (!dictionary.contains(term)) dict = term :: dict
      }
      dict
    }.toList.distinct
  }

  def parseHtml(texts: List[String]) = {
    texts.map { text =>
      Jsoup.parse(text).body().text()
    }
  }

  def retrievePostsIds(n: Int, pageLength: Int) = {
    inTransaction {
    	val ids = from(posts)(p => select(p.id) orderBy (p.id asc)).page(pageLength*n, pageLength)
    			println(ids.size + " post ids retrieved")
    			ids.toList    
    }
  }

  def retrieveCommentsIds(n: Int, pageLength: Int) = {
    val ids = from(comments)(c => select(c.id) orderBy (c.id asc)).page(pageLength*n, pageLength)
    println(ids.size + " comment ids retrieved")
    ids.toList
  }

  /**
   * Retrieve post title, and body from DB
   */
  def retrievePosts(ids: List[Int]) = {
    inTransaction {
      val postsQuery = from(posts)(p => where(p.id in ids) select (p.title, p.body))
      val texts = postsQuery.map {
        case (title, body) =>
          val ti = matchString(title)
          ti ++ " " ++ body
      }.toList
      //println(texts.size + " posts retrieved")
      parseHtml(parseHtml(texts))
    }
  }

  /**
   * Retrieve comment text from DB
   */
  def retrieveComments(ids: List[Int]) = {
    inTransaction {
    	val texts = inTransaction {
    		from(comments)(c => where(c.id in ids) select(c.text))
    	}.toList
    	parseHtml(texts)      
    }
  }
  
  def retrieveDictionary() = {
    inTransaction {
      from(terms)(t => select(t.term))
    }.map { x => (x, 0) }.toMap
  }

  /**
   * Extract the string from an Option[String]
   * @param s the Option[String]
   */
  def matchString(s: Option[String]) = {
    s match {
      case Some(n) => n
      case None => ""
    }
  }

}
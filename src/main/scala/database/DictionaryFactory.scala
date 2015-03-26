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

    //val texts = retrievePosts(url, username, password)
      val texts = "<p>The only way to do this in client side is using AJAX.<br> It is better to do SF stuff in server side (via DB if you have access or Web service if they have one or at least sending data via CURL at server side).<br> it is possible one of submissions fail in client side (and it seems to be bad if second one fail - first request has no idea that scond one is not done properly).</p> <hr> <p><strong>EDIT:</strong><br> Sample code for posting with CURL:<br> assuming you have a form with a <code>text</code> and a <code>hidden</code> input: </p> <pre><code>&lt;form method='post' action='http://your.sf.site/your_sf_submit_page.php'&gt; &lt;input type='text' name='field1'/&gt; &lt;input type='hidden' name='field2'/&gt; &lt;/form&gt; </code></pre> <p>you can submit it using CURL like this: </p> <pre><code>$ch = curl_init(); curl_setopt($ch, CURLOPT_URL,\"http://your.sf.site/your_sf_submit_page.php\"); curl_setopt($ch, CURLOPT_POST, 1); curl_setopt($ch, CURLOPT_POSTFIELDS,\"field1=value1&amp;field2=value2\"); curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); $server_output = curl_exec ($ch); curl_close ($ch); </code></pre> <p>then you can check <code>$server_output</code> for being sure that form is submitted correctly. </p>"
    
    val analyzer = new DefaultLuceneAnalyzer(Version.LUCENE_4_10_4, StopWords.asCharArraySet)
    
    val tokens = analyzer.createComponents("field", new StringReader(texts)).getTokenStream
    val attr = tokens.addAttribute(classOf[CharTermAttribute])
    
    retrieveSomething(url, username, password)
    
    //insertTerm(url, username, password, List())
    
    tokens.reset()
    
    while(tokens.incrementToken) {
      println(attr.toString())
    }

  }
  
  def parseHtml(texts: List[String]) = {
    texts.map { text => 
      Jsoup.parse(text).body().text()  
    }
  }
  
  def retrieveSomething(url: String, username: String, password: String) = {
    val cpds = DatabaseConnection.openConnection(url, username, password)

    val texts = inTransaction {
      val t = from(terms)(p => select(p.term))
      println(t.toList)
    }
    
    
    cpds.close()
  }

  /**
   * Retrieve post title, tags, and body from DB
   */
  def retrievePosts(url: String, username: String, password: String) = {
    val cpds = DatabaseConnection.openConnection(url, username, password)

    val texts = inTransaction {
      val postsId = from(posts)(p => select(p.id)).take(100)
      val postsQuery = from(posts)(p => where(p.id in postsId)select(p.title, p.body))
      postsQuery.map {
        case (title,body) =>
          val ti = matchString(title)
          ti ++ " " ++ body
      }.toList
    }
    cpds.close()
    parseHtml(texts)
  }

   /**
   * Retrieve comment text from DB
   */
  def retrieveComments(url: String, username: String, password: String) = {
    val cpds = DatabaseConnection.openConnection(url, username, password)

    val texts = inTransaction {
      from(comments)(c => select(c.text)).take(100)
    }.toList
    cpds.close()
    texts
  }
  
  def insertTerm(url: String, username: String, password: String, words: List[String]) = {
    val cpds = DatabaseConnection.openConnection(url, username, password)
    
    val term = inTransaction {
      //words.foreach { word => terms.insert(new Dictionary(word)) }
      terms.insert(new Dictionary("test"))
    }
    
    cpds close()
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
package database

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.SessionFactory
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.Session
import squeryl.StackExchangeSchema._
import com.mchange.v2.c3p0._
import squeryl.Post
import squeryl.Comment
import squeryl.Post2Tag
import scala.collection.immutable.TreeSet
import org.jsoup.Jsoup

object DatabaseRequest {

  /**
   * Opens a connection with a database
   *
   * @param url the address of the database
   * @param username
   * @param password
   */
  def openConnection(url: String, username: String, password: String) = {
    val cpds = new ComboPooledDataSource()
    cpds.setDriverClass("org.postgresql.Driver") //loads the jdbc driver 
    cpds.setJdbcUrl(url)
    cpds.setUser(username)
    cpds.setPassword(password);

    Class.forName("org.postgresql.Driver");
    SessionFactory.concreteFactory = Some(() =>
      Session.create(
        cpds.getConnection(username, password),
        new PostgreSqlAdapter))

    cpds.setMaxPoolSize(50)

    println("Opened DB")
    cpds
  }

  /**
   * Retrieve the posts that are questions.
   */
  def retrieveQuestionsIds(n: Int, pageLength: Int) = {
    inTransaction {
      from(posts)(p => where(p.postTypeId === 1) select (p.id) orderBy (p.creationDate asc)).page(pageLength * n, pageLength)
    }.toSet
  }

  /**
   * Retrieve all the posts that are questions.
   */
  def retrieveQuestionsIds() = {
    inTransaction {
      from(posts)(p => where(p.postTypeId === 1) select (p.id) orderBy (p.creationDate asc))
    }.toSet
  }

  /**
   * Retrieve the posts in a specific set of ids
   */
  def retrieveSpecificIds(n: Int, pageLength: Int, keyword: String) = {
    inTransaction {
      from(posts)(p => where((p.postTypeId === 1) and (p.tags like keyword)) select (p.id) orderBy (p.creationDate asc)).page(n, pageLength)
      //from(posts)(p => where((p.postTypeId === 1)) select(p.id)).page(n, pageLength)
    }.toSet
  }

  /**
   * Retrieve questions with respective comments
   */
  def retrieveQuestionsAndComments(ids: Set[Int]) = {
    inTransaction {
      val questionsAndAnswers = join(posts, comments.leftOuter)((p, c) =>
        where(p.id in ids)
          select (p, c)
          on (p.id === c.map(_.postId)))
      mapPostsWithComments(questionsAndAnswers.toList)
    }.toMap
  }

  /**
   * Retrieve answers with respective comments
   */
  def retrieveAnswersAndComments(ids: Set[Int]) = {
    inTransaction {
      val answersAndComments = join(posts, comments.leftOuter)((p, c) =>
        where(((p.parentId in ids)) and (p.postTypeId === 2))
          select (p, c)
          on (p.id === c.map(_.postId)))
      mapPostsWithComments(answersAndComments.toList)
    }.toMap
  }

  /**
   * Map posts with comments
   */
  def mapPostsWithComments(postsAndComments: List[(Post, Option[Comment])]) = {
    val posts = postsAndComments.map { case (post, comment) => (post.id, post) }.toMap
    val idsAndComments = postsAndComments.groupBy {
      case (post, comment) => post.id
    }.mapValues {
      pc => pc.flatMap { case (post, comment) => comment }
    }
    idsAndComments.map { case (id, comments) => (posts.get(id).get, comments) }
  }

  /**
   * Retrieve Tags
   */
  def retrieveTagsPosts(ids: Set[Int]) = {
    inTransaction {
      val tags = from(posts)(p => where((p.id in ids) and (p.postTypeId === 1)) select (p.id, p.tags)).toMap
      tags.map {
        case (id, tags) =>
          val ts = Jsoup.parse(tags.get).body().text().replace("<", "").replace(">", " ").split(" ").toList
          (id, ts)
      }
    }
  }

  /**
   * Insert tags in table
   */
  def insertTags(postsTags: Map[Int, List[String]]) = {
    inTransaction {
      val p2t = postsTags.map{ case(id, tags) => new Post2Tag(id, tags.mkString(" "))}.toList
      println("Ready to insert")
      post2tag.insert(p2t)
    }
  }
  
  /**
   * Retrieve tag2post
   */
   def retrieveTag2Post() = {
     inTransaction {
       from(post2tag)(pt => select(pt.id, pt.tags)).page(0, 10000)
     }.toMap.map{ case(id, tags) => (id, tags.split(" ").toList)}
   }
   
   /**
    * Get occurrences of a tag
    */
   def getTagOccurrences(tag: String) = {
     val occurrence = inTransaction {
       from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;"+tag+"&gt;%")) select(p.id)).toList.size
     }
     (tag, occurrence)
   }
}
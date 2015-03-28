package database

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.SessionFactory
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.Session
import squeryl.StackExchangeSchema._
import com.mchange.v2.c3p0._
import squeryl.Post
import squeryl.Comment
import scala.collection.immutable.TreeSet
import squeryl.ContextVector

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
      from(posts)(p => where(p.postTypeId === 1) select (p.id)).page(pageLength*n, pageLength)
    }.toSet
  }

  /**
   * Retrieve the dictionary from the database
   */
  def retrieveDictionary() = {
    inTransaction {
      from(terms)(t => select(t.term))
    }.toSet
  }

  /**
   * Retrieve questions with respective comments
   */
  def retrieveQuestionsAndComments(ids: Set[Int]) = {
    inTransaction {
    	val questionsAndAnswers = inTransaction {
    		join(posts, comments.leftOuter)((p, c) =>
    		where(p.id in ids)
    		select (p, c)
    		on (p.id === c.map(_.postId)))
    	}.toList
    	mapPostsWithComments(questionsAndAnswers)      
    }
  }

  /**
   * Retrieve answers with respective comments
   */
  def retrieveAnswersAndComments(ids: Set[Int]) = {
    inTransaction {
    	val answersAndComments = inTransaction {
    		join(posts, comments.leftOuter)((p, c) =>
    		where(((p.parentId in ids)) and (p.postTypeId === 2))
    		select (p, c)
    		on (p.id === c.map(_.postId)))
    	}.toList
    	mapPostsWithComments(answersAndComments)     
    }
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
    idsAndComments.map{case(id, comments) => (posts.get(id).get, comments)}
  }
  
  def insertContextVectors(contexts: List[ContextVector]) ={
    contextVectors.insert(contexts)
  }

}
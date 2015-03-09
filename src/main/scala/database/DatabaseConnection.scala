package database

import org.squeryl.SessionFactory
import org.squeryl.adapters.PostgreSqlAdapter
import org.squeryl.Session
import org.squeryl.PrimitiveTypeMode._
import squeryl.StackExchangeSchema._
import com.mchange.v2.c3p0._
import squeryl._
import org.jsoup._
import com.github.tototoshi.csv._
import java.io.File
import java.io.PrintWriter
import java.nio.charset.Charset

object DatabaseConnection {

  def main(args: Array[String]) {
    val strings = fetchDiscussions()
  }

  def openConnection() = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

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

  def fetchDiscussions() = {
    val path = "discussions.csv"
    val cpds = openConnection()
    var discussionsStrings: List[String] = Nil

    //Squeryl database interaction must occur in a transaction block :
    inTransaction {
      //val ids = from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;java&gt;%") and (p.id lt 50000)) select (p.id))
      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.id lt 100000)) select (p.id)).page(0, 1)

      println("# of ids: " + ids.distinct.size)

      val questionsWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where((p.id in ids) and (p.acceptedAnswerId > 0))
            select (p, c)
            on (p.id === c.map(_.postId)))

      val answersWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where((p.parentId in ids) and (p.postTypeId === 2))
            select (p, c)
            on (p.id === c.map(_.postId))).par

      val discussions = buildDiscussions(questionsWithComment.toList, answersWithComment.toList)

      discussionsStrings = discussions.map { x => getDiscussionString(x) }.toList
      println("# of discussions: " + discussionsStrings.size)
    }

    cpds.close()
    println("Closed DB")
    writeCSV(discussionsStrings, path)
    discussionsStrings
  }

  def buildDiscussions(questions: List[(Post, Option[Comment])], answers: List[(Post, Option[Comment])]) = {
    val questionsAndComment = mapPostsWithComments(questions)
    val answersAndComments = mapPostsWithComments(answers)
    val groupOfAnswers = answersAndComments.groupBy {
      case (x, y) => x.parentId match {
        case None => -1
        case Some(n) => n
      }
    }

    questionsAndComment.map {
      case (x, y) => new Discussion(x, y, groupOfAnswers.get(x.id) match {
        case None => null
        case Some(n) => n
      })
    }
  }

  def mapPostsWithComments(posts: List[(Post, Option[Comment])]) = {
    val idsAndComments = posts.groupBy { case (x, y) => x.id }.mapValues(y => y.map { case (x, y) => y })
    idsAndComments.map { case (x, y) => (getPost(posts, x), y) }
  }

  def getPost(posts: List[(Post, Option[Comment])], id: Int) = {
    posts.find { case (x, y) => x.id == id } match {
      case None => null // This should never happen
      case Some(n) => n._1
    }
  }

  def getDiscussionString(discussion: Discussion) = {
    val initialParse = Jsoup.parse(discussion.toString())
    val initialString = initialParse.body().text()
    // At this point the tags that were in the form of lt;p;gt are transormed in <p>
    val doc = Jsoup.parse(initialString)
    doc.body().text()
  }

  def writeCSV(strings: List[String], path: String) = {
    val stringsWithoutCommas = strings.map { x => x.split(",").toList.mkString("") }
    val content = stringsWithoutCommas.map { x => List(stringsWithoutCommas.indexOf(x), "A", x) }
    
    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(content)
    writer.close()
  }

}
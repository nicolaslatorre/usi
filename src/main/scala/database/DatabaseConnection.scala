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
    //    val strings = fetchDiscussions()
    val strings = fetchTags
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
    val path = "datasets/dataset5/discussions-random.csv"
    val cpds = openConnection()

    //Squeryl database interaction must occur in a transaction block :
    val discussions = inTransaction {
      //      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;java&gt;%") and (p.id lt 100000)) select (p.id))

      //      val ids = from(posts)(p => where((p.postTypeId === 1) and ((p.tags like "%&lt;html&gt;%") or (p.tags like "%&lt;java&gt;%")
      //        or (p.tags like "%&lt;c++&gt;%") or (p.tags like "%&lt;javascript&gt;%") or (p.tags like "%&lt;python&gt;%")) and (p.id lt 100000)) select (p.id))

      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.id lt 50000)) select (p.id))

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

      //      discussionsStrings = discussions.map { x => getDiscussionString(x) }.toList
      //      println("# of discussions: " + discussionsStrings.size)
      discussions
    }.toList

    cpds.close()
    println("Closed DB")
    writeCSV(discussions, path)
    discussions.map { x => getDiscussionString(x) }.toList
  }

  //  def fetchTags = {
  //    val path = "discussions-tags.csv"
  //    val cpds = openConnection()
  //    val discussionsStrings = inTransaction {
  //      val ids = from(posts)(p => where((p.postTypeId === 1) and ((p.tags like "%&lt;html&gt;%") or (p.tags like "%&lt;java&gt;%")
  //        or (p.tags like "%&lt;c++&gt;%") or (p.tags like "%&lt;javascript&gt;%") or (p.tags like "%&lt;python&gt;%")) and (p.id lt 100000)) select (p.id))
  //
  //      println("# of ids: " + ids.distinct.size)
  //
  //      val questionsWithComment =
  //        join(posts, comments.leftOuter)((p, c) =>
  //          where((p.id in ids) and (p.acceptedAnswerId > 0))
  //            select (p, c)
  //            on (p.id === c.map(_.postId)))
  //
  //      val answersWithComment =
  //        join(posts, comments.leftOuter)((p, c) =>
  //          where((p.parentId in ids) and (p.postTypeId === 2))
  //            select (p, c)
  //            on (p.id === c.map(_.postId))).par
  //
  //      buildTagsMatrix(questionsWithComment.toList, answersWithComment.toList).toList
  //    }
  //    println("# of discussions: " + discussionsStrings.size)
  //
  //    cpds.close()
  //    println("Closed DB")
  //
  //    val file = new File(path)
  //    val writer = CSVWriter.open(file)
  //    writer.writeAll(discussionsStrings)
  //    writer.close()
  //    discussionsStrings
  //  }

  def fetchTags = {
    val path = "discussions-dataset5.csv"
    val cpds = openConnection()
    val discussionsStrings = inTransaction {
//      val ids = from(posts)(p => where((p.postTypeId === 1) and ((p.tags like "%&lt;php&gt;%") and (p.tags like "%&lt;sql&gt;%") and
//        (p.tags like "%&lt;database&gt;%") ) and (p.id lt 4000000)) select (p.id))
        
//        val ids = from(posts)(p => where((p.postTypeId === 1) and ((p.tags like "%&lt;java&gt;%") and (p.tags like "%&lt;swing&gt;%")
//            and (p.id lt 300000))) select (p.id))
//        
//        val ids = from(posts)(p => where((p.postTypeId === 1) and ((p.tags like "%&lt;python&gt;%") and (p.tags like "%&lt;numpy&gt;%") 
//            and (p.id lt 2000000))) select (p.id))
//        
//        val ids = from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;c++&gt;%") and (p.tags like "%&lt;opengl&gt;%") 
//            and (p.id lt 2000000)) select (p.id))
      
      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;c#&gt;%") and (p.tags like "%&lt;.net&gt;%") 
            and (p.id lt 30000)) select (p.id))
      
      

      println("# of ids: " + ids.distinct.size)
//      println("# of ids2: " + ids2.size)
//      println("# of ids3: " + ids3.size)
//      println("# of ids4: " + ids4.size)

      val questionsWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.id in ids)) and (p.acceptedAnswerId > 0))
            select (p, c)
            on (p.id === c.map(_.postId)))

      val answersWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.parentId in ids)) and (p.postTypeId === 2))
            select (p, c)
            on (p.id === c.map(_.postId))).par

//      buildTagsMatrix(questionsWithComment.toList, answersWithComment.toList).toList
      val discussions = buildDiscussions(questionsWithComment.toList, answersWithComment.toList)
      discussions
    }.toList
    
    println("# of discussions: " + discussionsStrings.size)
    cpds.close()
    println("Closed DB")

    val file = new File(path)
    val writer = CSVWriter.open(file)
    writeCSV(discussionsStrings, path)
    discussionsStrings
  }

  def buildTagsMatrix(questions: List[(Post, Option[Comment])], answers: List[(Post, Option[Comment])]) = {
    //    val tags = Map()
    val questionsAndComment = mapPostsWithComments(questions)
    val answersAndComments = mapPostsWithComments(answers)
    val groupOfAnswers = answersAndComments.groupBy {
      case (x, y) => x.parentId match {
        case None => -1
        case Some(n) => n
      }
    }

    questionsAndComment.map {
      case (x, y) => getTags(x)
    }
  }

  def getTags(post: Post) = {
    val tags = post.tags match {
      case Some(t) => t
      case None => ""
    }
    val postTags = Jsoup.parse(tags).body().text().replace("<", "").replace(">", " ").split(" ").toList

    val tagList = List("html", "c++", "java", "javascript", "python")
    val t = postTags.filter { x => tagList.contains(x) }.map { x =>
      x match {
        case "html" => List(1, 0, 0, 0, 0)
        case "c++" => List(0, 1, 0, 0, 0)
        case "java" => List(0, 0, 1, 0, 0)
        case "javascript" => List(0, 0, 0, 1, 0)
        case "python" => List(0, 0, 0, 0, 1)
        case _ => List(0, 0, 0, 0, 0)
      }
    }

    t.foldLeft(List(0, 0, 0, 0, 0))((x, y) => x zip y map {
      case (x, y) => (x > y) match {
        case true => x
        case false => y
      }
    })

  }

  def buildTags(post: Post) = {
    val tags = post.tags match {
      case Some(t) => t
      case None => ""
    }
    Jsoup.parse(tags).body().text().replace("<", "").replace(">", " ").split(" ").toList
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
      case (x, y) => new Discussion(x.id, x, y, groupOfAnswers.get(x.id) match {
        case None => Map(new Post -> List(Some(new Comment())))
        case Some(n) => n
      }, buildTags(x))
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

  def writeCSV(discussions: List[Discussion], path: String) = {
    val strings = discussions.map { x => (x.id, getDiscussionString(x), x.tags) }.toList
    val stringsWithoutCommas = strings.map { case (x, y, z) => (x, y.split(",").toList.mkString(""), z) }
    val content = stringsWithoutCommas.map { case (x, y, z) => List(x, "A", y, z.mkString(" ")) }

    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(content)
    writer.close()
  }

}
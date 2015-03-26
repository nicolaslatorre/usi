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
import scala.io.Source

object DatabaseConnection {

  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val path = "../Datasets/dataset3/10-discussions-tags.csv"
    val size = 10 //args(1).toInt

    val discussions = buildByTags(url, username, password, size, path, true)
  }

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

//  def fetchFullDiscussions(url: String, username: String, password: String, path: String, idsPath: String) = {
//    val cpds = openConnection(url, username, password)
//    val discussions = inTransaction {
//
//      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0)) select (p.id)).take(5000)
//
//      val questionsWithComment =
//        join(posts, comments.leftOuter)((p, c) =>
//          where(((p.id in ids)))
//            select (p, c)
//            on (p.id === c.map(_.postId)))
//
//      val answersWithComment =
//        join(posts, comments.leftOuter)((p, c) =>
//          where(((p.parentId in ids)) and (p.postTypeId === 2))
//            select (p, c)
//            on (p.id === c.map(_.postId)))
//
//      buildFullDiscussions(questionsWithComment.toList, answersWithComment.toList)
//    }.toList
//
//    println("# of discussions: " + discussions.size)
//    cpds.close()
//    println("Closed DB")
//    writeCSV(discussions, path)
//    discussions
//  }
//
//  def fetchQuestionsAndAnswers(url: String, username: String, password: String, path: String, idsPath: String) = {
//    val cpds = openConnection(url, username, password)
//    val discussions = inTransaction {
//
//      val ids = Source.fromFile(new File(idsPath)).getLines().toList.map { x => x.toInt }
//
//      val questions = from(posts)(p => where(p.id in ids) select (p)).toList
//      val answers = from(posts)(p => where((p.parentId in ids) and (p.postTypeId === 2)) select (p)).toList
//
//      buildQuestionsAndAnswers(questions, answers)
//    }.toList
//
//    println("# of discussions: " + discussions.size)
//    cpds.close()
//    println("Closed DB")
//    writeCSV(discussions, path)
//    discussions
//  }

  def fetchQuestions(url: String, username: String, password: String, path: String, idsPath: String) = {
    val cpds = openConnection(url, username, password)
    val discussions = inTransaction {

      val ids = Source.fromFile(new File(idsPath)).getLines().toList.map { x => x.toInt }

      val questions = from(posts)(p => where(p.id in ids) select (p)).toList

      buildQuestions(questions)
    }.toList

    println("# of discussions: " + discussions.size)
    cpds.close()
    println("Closed DB")
    writeCSV(discussions, path)
    discussions

  }

  def buildSingleRecurrences(path: String, newPath: String) = {
    val tags = Source.fromFile(new File(path)).getLines().toList

    val newTags = tags.map { x => x.split(",").toList }.filter { x => x(0).split(" ").length == 1 }
    val t = newTags.map { x => (List(x(0)), x(1).toInt) }

    writeTagsCSV(t, newPath)
    //println(newTags.take(3))
  }

  def buildByTags(url: String, username: String, password: String, size: Int, path: String = "", save: Boolean = false) = {
    val cpds = openConnection(url, username, password)
    val matrix = inTransaction {

      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0)) select (p.id) orderBy (p.creationDate asc)).take(size)

      //      val questionsWithComment =
      //        join(posts, comments.leftOuter)((p, c) =>
      //          where(((p.id in ids)))
      //            select (p, c)
      //            on (p.id === c.map(_.postId)))

      val questions = from(posts)(p => where(p.id in ids) select (p)).toList

      //      val answersWithComment =
      //        join(posts, comments.leftOuter)((p, c) =>
      //          where(((p.parentId in ids)) and (p.postTypeId === 2))
      //            select (p, c)
      //            on (p.id === c.map(_.postId)))

      val discussions = buildQuestions(questions.toList)
      //      val discussions = buildFullDiscussions(questionsWithComment.toList, answersWithComment.toList).toList
      val matrix = buildTagsMatrix(discussions.sortBy { x => x.creationDate }).toList

      matrix
    }

    println("# of discussions: " + matrix.size)
    cpds.close()
    println("Closed DB")
    if (save) {
      writeTagsMatrixCSV(matrix, path)
      println("Saved")
    }
    //matrix
    //getTagsMatrix(matrix)
  }

  def buildRecurrences(url: String, username: String, password: String, path: String) = {
    val cpds = openConnection(url, username, password)
    val tagsList = inTransaction {

      //      val tags = from(posts)(p => where((p.postTypeId === 1)) select (p.tags))

      val tags = from(posts)(p => where((p.postTypeId === 1) and (p.tags like "&lt;%&gt;")) select (p.tags))

      tags.map {
        case (Some(tag)) => buildTags(tag).sortWith(_.toLowerCase < _.toLowerCase)
        case None => List("")
      }.groupBy(x => x).mapValues { y => y.size }

    }.toList.sortBy(x => x._2).reverse

    println("# of tags: " + tagsList.size)
    cpds.close()
    println("Closed DB")
    writeTagsCSV(tagsList, path)
    tagsList
  }

  def buildTagsMatrix(discussions: List[Discussion]) = {
    val tags = discussions.flatMap { discussion => discussion.tags }.distinct

    discussions.map {
      case x =>
        //        val vectorTag = TagManager.getVectorTag(x.tags, tagsOccurences)
        val vectorTag = TagManager.buildVectorTag(x.tags, tags)
        (x, vectorTag)
    }
  }

  def getTagsOccurences(path: String) = {
    Source.fromFile(new File(path)).getLines().map { x =>
      val Array(tag, value) = x.split(",")
      (tag, value.toInt)
    }.toList
  }

  def buildTags(post: Post) = {
    val tags = post.tags match {
      case Some(t) => t
      case None => ""
    }
    Jsoup.parse(tags).body().text().replace("<", "").replace(">", " ").split(" ").toList
  }

  def buildTags(tags: String) = {
    Jsoup.parse(tags).body().text().replace("<", "").replace(">", " ").split(" ").toList
  }

  //  def buildFullDiscussions(questions: List[(Post, Option[Comment])], answers: List[(Post, Option[Comment])]) = {
  //    val questionsAndComment = mapPostsWithComments(questions)
  //    val answersAndComments = mapPostsWithComments(answers)
  //    val groupOfAnswers = answersAndComments.groupBy {
  //      case (x, y) => x.parentId match {
  //        case None => -1
  //        case Some(n) => n
  //      }
  //    }
  //
  //    questionsAndComment.map {
  //      case (x, y) => new Discussion(x.id, x.title.get, x, y, groupOfAnswers.get(x.id) match {
  //        case None => Map(new Post -> List(Some(new Comment())))
  //        case Some(n) => n
  //      }, buildTags(x))
  //    }
  //  }
  //
  //  def buildQuestionsAndAnswers(questions: List[Post], answers: List[Post]) = {
  //    val idQuestions = questions.map { x => (x.id, x) }
  //    val idAnswers = answers.groupBy { x => x.parentId.get } //map { x => (x.parentId.get, x) }
  //    val questionsAndAnswers = idQuestions.zip(idAnswers).groupBy(_._1).mapValues { x => x.map { case (q, a) => (q._2, a._2) } }.flatMap { case (x, y) => y }.toList
  //    questionsAndAnswers.map { case (q, a) => new Discussion(q.id, q.title.get, q, List(), a.map { x => (x, List()) }.toMap, buildTags(q)) }
  //  }

  def buildQuestions(questions: List[Post]) = {
    val idQuestions = questions.map { x => (x.id, x) }
    questions.map { q => new Discussion(q.id, q.title, q.creationDate, q.body, q.commmentCount, q.answerCount, List(), Map(), buildTags(q)) }
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

  def writeTagsMatrixCSV(matrix: List[(Discussion, List[Int])], path: String) = {
    println("Writing...")
    val firstRow = (for (i <- 0 until matrix(0)._2.size) yield "T" + i).toList
    val file = new File(path)
    val writer = CSVWriter.open(file)

    writer.writeRow("" :: (firstRow ::: List("Text")))

    val contents = matrix.map {
      case (discussion, distribution) =>
        val id = discussion.id.toString
        val title = discussion.title match {
          case Some(n) => n
          case None => ""
        }
        val date = discussion.creationDate.toString()

        val commentCount = discussion.commentCount match {
          case Some(n) => n.toString
          case None => "0"
        }

        val answerCount = discussion.answerCount match {
          case Some(n) => n.toString
          case None => "0"
        }
        
        val text = getDiscussionString(discussion)
        val tags = discussion.tags.mkString(" ")

        //val string = (discussion.id.toString, getDiscussionString(discussion).split(",").mkString(""), discussion.tags.mkString(" "))
        val ds = distribution.zipWithIndex.filter { x => x._1 == 1 }.map { case (value, index) => value + " " + index }
        //(string._1 :: string._2 :: string._3 :: discussion.question.answerCount.get.toString :: discussion.question.commmentCount.get.toString :: ds)
        (id :: title :: text :: tags :: date :: answerCount :: commentCount :: ds)
    }

    contents.foreach {
      x => writer.writeRow(x)
    }

    writer.close()
  }

//  def getTagsMatrix(matrix: List[(Discussion, List[Int])]) = {
//    val firstRow = (for (i <- 0 until matrix(0)._2.size) yield "T" + i).toList
//    val contents = matrix.map {
//      case (discussion, distribution) =>
//        val string = (discussion.id.toString, getDiscussionString(discussion).split(",").mkString(""), discussion.tags.mkString(" "))
//        val ds = distribution.zipWithIndex.filter { x => x._1 == 1 }.map { case (value, index) => value + " " + index }
//        (string._1 :: string._2 :: string._3 :: discussion.question.answerCount.get.toString :: discussion.question.commmentCount.get.toString :: ds)
//    }
//
//    ("" :: (firstRow ::: List("Text"))) :: contents
//  }

  def writeTagsCSV(tags: List[(List[String], Int)], path: String) = {
    val content = tags.map { case (tag, r) => List(tag.mkString(" "), r) }
    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(content)
    writer.close()
  }

}
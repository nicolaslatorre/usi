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

    //    val path = "../Datasets/dataset7/discussions-fullQuestions.csv"
    //    val path = "../Datasets/dataset7/discussions-nocomment.csv"
    //    val path = "../Datasets/dataset7/discussions-questions.csv"

    val path = "../Datasets/dataset5/5000-discussions-tags.csv"

    val idsPath = "../Datasets/dataset2/ids.txt"
    val tagPath = "../Datasets/tags-recurrence.csv"
    val newTagPath = "../Datasets/tags-recurrence-single.csv"
    val singleTagsPath = "../Datasets/single-tags-recurrence.csv"

    //    val fullDiscussions = fetchFullDiscussions(url, username, password, path, idsPath)
    //    val questionsAndAnswer = fetchQuestionsAndAnswers(url, username, password, path, idsPath)
    //    val questions = fetchQuestions(url, username, password, path, idsPath)
    //      val tags = buildRecurrences(url, username, password, tagPath)

    //      val tags = buildSingleRecurrences(tagPath, newTagPath)

    val tagsOccurences = getTagsOccurences(singleTagsPath)

    val d = buildByTags(url, username, password, path, tagsOccurences.take(50))

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

  def fetchFullDiscussions(url: String, username: String, password: String, path: String, idsPath: String) = {
    val cpds = openConnection(url, username, password)
    val discussions = inTransaction {

      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0)) select (p.id)).take(5000)

      //      val ids1 = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and ((p.tags like "%&lt;php&gt;%") and (p.tags like "%&lt;sql&gt;%") and
      //        (p.tags like "%&lt;database&gt;%"))) select (p.id)).take(1500)
      //
      //      val ids2 = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and ((p.tags like "%&lt;java&gt;%") and (p.tags like "%&lt;swing&gt;%")
      //        and not(p.id in ids1))) select (p.id)).take(1500)
      //
      //      val ids3 = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and ((p.tags like "%&lt;python&gt;%") and (p.tags like "%&lt;numpy&gt;%")
      //        and not(p.id in ids1) and not(p.id in ids2))) select (p.id)).take(1500)
      //
      //      val ids4 = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and (p.tags like "%&lt;c++&gt;%") and (p.tags like "%&lt;opengl&gt;%")
      //        and not(p.id in ids1) and not(p.id in ids2) and not(p.id in ids3)) select (p.id)).take(1500)

      //      val ids = ids1.toList ::: ids2.toList ::: ids3.toList ::: ids4.toList

      //      val ids = Source.fromFile(new File(idsPath)).getLines().toList.map { x => x.toInt }

      val questionsWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.id in ids)))
            select (p, c)
            on (p.id === c.map(_.postId)))

      val answersWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.parentId in ids)) and (p.postTypeId === 2))
            select (p, c)
            on (p.id === c.map(_.postId)))

      buildFullDiscussions(questionsWithComment.toList, answersWithComment.toList)
    }.toList

    println("# of discussions: " + discussions.size)
    cpds.close()
    println("Closed DB")
    writeCSV(discussions, path)
    discussions
  }

  def fetchQuestionsAndAnswers(url: String, username: String, password: String, path: String, idsPath: String) = {
    val cpds = openConnection(url, username, password)
    val discussions = inTransaction {

      val ids = Source.fromFile(new File(idsPath)).getLines().toList.map { x => x.toInt }

      val questions = from(posts)(p => where(p.id in ids) select (p)).toList
      val answers = from(posts)(p => where((p.parentId in ids) and (p.postTypeId === 2)) select (p)).toList

      buildQuestionsAndAnswers(questions, answers)
    }.toList

    println("# of discussions: " + discussions.size)
    cpds.close()
    println("Closed DB")
    writeCSV(discussions, path)
    discussions
  }

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

  def buildByTags(url: String, username: String, password: String, path: String, tagsOccurences: List[(String, Int)]) = {
    val cpds = openConnection(url, username, password)
    val matrix = inTransaction {
      //      val tags = Source.fromFile(new File(tagPath)).getLines().toList.map { x => x.split(",").toList.head }

      //var ids: List[Int] = Nil

      //    val ids1 = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and (p.tags like "%&lt;"+x+";%"))) select (p.id)).take(1500)

      //      tags.take(50).map { x =>
      //        val idsTemp = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0) and ((p.tags like "&lt;" + x + "&gt;") and not(p.id in ids))) select (p.id)).take(100).toList
      //        ids = idsTemp ::: ids
      //      }
      //
      //      println("size: " + ids.size)
      //      println("distinct " + ids.distinct.size)

      val ids = from(posts)(p => where((p.postTypeId === 1) and (p.acceptedAnswerId > 0)) select (p.id)).take(5000)

      val questionsWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.id in ids)))
            select (p, c)
            on (p.id === c.map(_.postId)))

      val answersWithComment =
        join(posts, comments.leftOuter)((p, c) =>
          where(((p.parentId in ids)) and (p.postTypeId === 2))
            select (p, c)
            on (p.id === c.map(_.postId)))

      val discussions = buildFullDiscussions(questionsWithComment.toList, answersWithComment.toList).toList
      val matrix = buildTagsMatrix(discussions, tagsOccurences).toList

      matrix
    }

    println("# of discussions: " + matrix.size)
    cpds.close()
    println("Closed DB")
    writeTagsMatrixCSV(matrix, path)
    println("Saved")
    matrix
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

  def buildTagsMatrix(discussions: List[Discussion], tagsOccurences: List[(String, Int)]) = {
    discussions.map {
      case x =>
        val vectorTag = TagManager.getVectorTag(x.tags, tagsOccurences)
        (x, vectorTag)
    }
  }

  //  def buildTagsMatrix(questions: List[(Post, Option[Comment])], answers: List[(Post, Option[Comment])]) = {
  //    //    val tags = Map()
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
  //      case (x, y) => getTags(x)
  //    }
  //  }
  //
  //  def getTags(post: Post) = {
  //    val tags = post.tags match {
  //      case Some(t) => t
  //      case None => ""
  //    }
  //    val postTags = Jsoup.parse(tags).body().text().replace("<", "").replace(">", " ").split(" ").toList
  //
  //    val tagList = List("html", "c++", "java", "javascript", "python")
  //    val t = postTags.filter { x => tagList.contains(x) }.map { x =>
  //      x match {
  //        case "html" => List(1, 0, 0, 0, 0)
  //        case "c++" => List(0, 1, 0, 0, 0)
  //        case "java" => List(0, 0, 1, 0, 0)
  //        case "javascript" => List(0, 0, 0, 1, 0)
  //        case "python" => List(0, 0, 0, 0, 1)
  //        case _ => List(0, 0, 0, 0, 0)
  //      }
  //    }
  //
  //    t.foldLeft(List(0, 0, 0, 0, 0))((x, y) => x zip y map {
  //      case (x, y) => (x > y) match {
  //        case true => x
  //        case false => y
  //      }
  //    })
  //
  //  }

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

  def buildFullDiscussions(questions: List[(Post, Option[Comment])], answers: List[(Post, Option[Comment])]) = {
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

  def buildQuestionsAndAnswers(questions: List[Post], answers: List[Post]) = {
    val idQuestions = questions.map { x => (x.id, x) }
    val idAnswers = answers.groupBy { x => x.parentId.get } //map { x => (x.parentId.get, x) }
    val questionsAndAnswers = idQuestions.zip(idAnswers).groupBy(_._1).mapValues { x => x.map { case (q, a) => (q._2, a._2) } }.flatMap { case (x, y) => y }.toList
    questionsAndAnswers.map { case (q, a) => new Discussion(q.id, q, List(), a.map { x => (x, List()) }.toMap, buildTags(q)) }
  }

  def buildQuestions(questions: List[Post]) = {
    val idQuestions = questions.map { x => (x.id, x) }
    questions.map { q => new Discussion(q.id, q, List(), Map(), buildTags(q)) }
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
        val string = (discussion.id.toString, getDiscussionString(discussion).split(",").mkString(""), discussion.tags.mkString(" "))
        val ds = distribution.zipWithIndex.filter { x => x._1==1 }.map { case(value, index) => value + " " + index  }
        (string._1 :: string._2 :: string._3 :: discussion.question.answerCount.get.toString :: discussion.question.commmentCount.get.toString :: ds)
    }

    contents.foreach {
      x => writer.writeRow(x)
    }

    writer.close()
  }

  def writeTagsCSV(tags: List[(List[String], Int)], path: String) = {
    val content = tags.map { case (tag, r) => List(tag.mkString(" "), r) }
    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(content)
    writer.close()
  }

}
package database

import java.io.StringReader
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.jsoup.Jsoup
import squeryl.Comment
import squeryl.Post
import com.github.tototoshi.csv.CSVWriter
import java.io.File
import com.github.tototoshi.csv.CSVReader
import java.util.Date
import com.github.nscala_time.time.Imports._

object DataManagement {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val tags = List("c#")

    buildDiscussionsFiles(url, username, password)
    println("END")
  }

  def buildDiscussions(questions: Map[Post, List[Comment]], answers: Map[Post, List[Comment]]) = {
    val ids = answers.groupBy { case (post, comments) => post.parentId.get }

    questions.map {
      case (post, comments) =>
        val ans: List[Answer] = ids.get(post.id) match {
          case Some(n) => buildAnswers(n)
          case None => List()
        }
        val tags = buildTags(post.tags.get)
        val title = parseHtml(parseHtml(post.title.get))
        val body = parseHtml(parseHtml(post.body))
        new Discussion(post.id, title, post.creationDate, 0, 0, 0, 0, None, Nil)
    }.toList
  }

  def buildDiscussionsFiles(url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val discussions = DatabaseRequest.retrieveQuestionsInfo()
    cpds.close()

    val singleTags2infos = discussions.groupBy { discussion => discussion.tags.take(1) }

    singleTags2infos.par.map {
      case (single, values) =>
        val folder = "Discussions/" + single.mkString("") + "/"
        val tags2infos = values.groupBy { value => value.tags }

        tags2infos.map {
          case (tag, ds) =>
            val infos = ds.map { d => d.getInfo() }

            val savePath = folder + tag.mkString(" ") + ".csv"
            val file = new File(savePath)
            file.getParentFile.mkdirs()
            val writer = CSVWriter.open(file)
            writer.writeAll(infos)
            writer.close()
        }
    }
  }

  def openDiscussionsFiles(tags: List[String], ids: Set[Int]) = {
    val single = tags.take(1).mkString("")
    val path = "../Discussions/" + single + "/" + tags.mkString(" ") + ".csv"
    val file = new java.io.File(path)
    val reader = CSVReader.open(file)

    val lines = reader.iterator
    val discussions = lines.filter { line =>
      val id = line.head.toInt
      ids.contains(id)
    }.toList

    discussions.map { discussion =>
      val Array(id, title, creation, answers, score, view, owner, closed) = discussion.toArray
      val Array(year, month, day) = creation.split("-")
      val creationDate = new LocalDate(year.toInt, month.toInt, day.toInt)

      val closedDate = getClosedDate(closed)

      new Discussion(id.toInt, title, creationDate.toDate(), answers.toInt, score.toInt, view.toInt, owner.toInt, closedDate, tags)
    }
  }

  def getClosedDate(date: String) = {
    if (date == "Still Open") None
    else {
      val Array(year, month, day) = date.split("-")
      val closed = new LocalDate(year.toInt, month.toInt, day.toInt)
      Some(closed.toDate)
    }
  }

  /**
   * Builds answers
   */
  def buildAnswers(answers: Map[Post, List[Comment]]) = {
    answers.map {
      case (post, comments) =>
        val body = parseHtml(parseHtml(post.body))
        new Answer(post.id, post.parentId.get, post.creationDate, body, comments)
    }.toList
  }

  def getText(post: List[(Int, Option[String], String, Option[Comment])]) = {

  }

  /**
   * Build tags as a list of strings
   */
  def buildTags(tags: String) = {
    parseHtml(tags).replace("<", "").replace(">", " ").split(" ").toList
  }

  def parseHtml(text: String) = {
    Jsoup.parse(text).body().text()
  }
}
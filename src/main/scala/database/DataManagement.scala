package database

import java.io.StringReader

import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.jsoup.Jsoup
import squeryl.Comment
import squeryl.Post

object DataManagement {

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
        new Discussion(post.id, title, post.creationDate, None, 0, 0, 0, None)
    }.toList
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
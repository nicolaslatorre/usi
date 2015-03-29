package database

import squeryl.Post
import squeryl.Comment
import org.jsoup.Jsoup
import lucene.StopWords
import lucene.DefaultLuceneAnalyzer
import org.apache.lucene.util.Version
import java.io.StringReader
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute

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
        new Discussion(post.id, title, post.creationDate, body, post.commmentCount, post.answerCount, comments, ans, tags)
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

  /**
   * Build tags as a list of strings
   */
  def buildTags(tags: String) = {
    parseHtml(tags).replace("<", "").replace(">", " ").split(" ").toList
  }

  def parseHtml(text: String) = {
    Jsoup.parse(text).body().text()
  }

  /**
   * Get the set of terms of a discussion.
   */
  def getPostsTerms(analyzer: DefaultLuceneAnalyzer, questions: Map[Post, List[Comment]], answers: Map[Post, List[Comment]]) = {
    val ids = answers.groupBy { case (post, comments) => post.parentId.get }

    questions.par.map {
      case (post, comments) =>
        val answersTerms:Set[String] = ids.get(post.id) match {
          case Some(n) => getAnswersTerms(analyzer, n)
          case None => Set()
        }
        val title = post.title.get
        val body = post.body
        val upper = parseHtml(parseHtml(title ++ " " ++ body))
        val commentsString = comments.map { comment => parseHtml(comment.text) }.mkString(" ")
        val questionTerms = getTerms(analyzer, upper  ++ " " ++ commentsString)
        (post.id, questionTerms union answersTerms)
    }.toMap
  }

  def getAnswersTerms(analyzer: DefaultLuceneAnalyzer, answers: Map[Post, List[Comment]]) = {
    answers.flatMap {
      case (post, comments) =>
        val body = parseHtml(parseHtml(post.body))
        val commentsString = comments.map { comment => parseHtml(comment.text) }.mkString(" ")
        getTerms(analyzer, body ++ " " ++ commentsString)
    }.toSet
  }

  def getTerms(analyzer: DefaultLuceneAnalyzer, text: String) = {
    var terms: Set[String] = Set()
    val tokens = analyzer.createComponents("field", new StringReader(text)).getTokenStream
    val attr = tokens.addAttribute(classOf[CharTermAttribute])

    tokens.reset()

    while (tokens.incrementToken) {
      val term = attr.toString()
      terms = terms + term
    }
    terms
  }
}
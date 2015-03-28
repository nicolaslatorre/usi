package database

import squeryl._
import java.util.Date

class Discussion(val id: Int, val title: String, val creationDate: Date, val body: String, val commentCount: Option[Int], val answerCount: Option[Int], val comments: List[Comment], val answers: List[Answer], val tags: List[String]) {
  
  override def toString() = {
    val commentsString = comments.map{comment => comment.text }.mkString("\n")

    val answersString = answers.map { answer => answer.toString() }.mkString("\n")

    title + "\n" + body + "\n" + commentsString + "\n" + answersString + "\n"
  }

}
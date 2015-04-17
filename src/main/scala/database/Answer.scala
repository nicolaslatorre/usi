package database

import java.util.Date
import squeryl.Comment

class Answer(val id: Int, val parentId: Int, val creationDate: Date, val body: String, val comments: List[Comment]) {
  
  override def toString() = {
    val commentsString = comments.map{comment => comment.text}.mkString("\n")
    
    body ++ "\n" ++ commentsString
  }
  
  def getFreshCreationDate() = {
    val commentsDates = comments.map{ comment => comment.creationDate}
    (creationDate :: commentsDates).max
  }

}
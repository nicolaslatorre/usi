package database

import squeryl._
import java.util.Date
import com.github.nscala_time.time.Imports._

class Discussion(val id: Int, val title: String, val creationDate: Date, val answers: Int, val score: Int, 
    val viewCount: Int, val ownerId: Int, val closedDate: Option[Date], val tags: List[String]) {
  
  override def toString() = {
    val closed = closedDate match {
      case None => "Still Open"
      case Some(date) => date.toString
    }
    title + " " + " " + creationDate.toString() + " " + answers.toString + " " + score.toString() + " " + viewCount.toInt + 
    " " + ownerId + " " + closed + " " + id.toString
  }
  
  def getInfo() = {
    val closed = closedDate match {
      case None => "Still Open"
      case Some(date) => date.toLocalDate.toString()
    }
    
    List(id.toString(), title, creationDate.toLocalDate.toString(), answers.toString(), score.toString(), viewCount.toString(), ownerId.toString(), closed)
  }
  

}
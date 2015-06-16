package database

import squeryl._
import java.util.Date

class Discussion(val id: Int, val title: String, val creationDate: Date, val answerCount: Option[Int], val score: Int, 
    val viewCount: Int, val ownerId: Int, val closedDate: Option[Date]) {
  
  override def toString() = {
    val closed = closedDate match {
      case None => "Still Open"
      case Some(date) => date.toString
    }
    title + " " + " " + creationDate.toString() + " " + answerCount.get.toString + " " + score.toInt + " " + viewCount.toInt + 
    " " + ownerId + " " + closed + " " + id.toString
  }
  

}
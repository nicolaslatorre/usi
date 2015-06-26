package database

import java.util.Date

import com.github.nscala_time.time.Imports.richDate

class Discussion(val id: Int, val title: String, val creationDate: Date, val answers: Int, val score: Int,
  val viewCount: Int, val ownerId: Int, val tags: List[String]) {

  override def toString() = {

    title + " " + " " + creationDate.toString() + " " + answers.toString + " " + score.toString() + " " + viewCount.toInt +
      " " + ownerId + " " + id.toString
  }

  def getInfo() = {

    List(id.toString(), title, creationDate.toLocalDate.toString(), answers.toString(), score.toString(), viewCount.toString(), ownerId.toString())
  }

}
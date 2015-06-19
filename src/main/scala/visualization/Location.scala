package visualization

import multidimensionalscaling.Document
import java.util.Date
import com.github.nscala_time.time.Imports._
import database.Tag

case class Location(val tags: List[String], val total: Double, var dates2ids: Map[LocalDate, (Int, Stream[Int])], 
    val rectangle: Option[ScalaRectangle], var internalRectangle: Option[ScalaRectangle], var selected: Boolean) {

  /**
   * Get tags as a string
   */
  def getTagsAsString() = {
    tags.mkString(" ")
  }

  def getRectangle() = {
    rectangle.getOrElse(null)
  }

  def getInternalRectangle() = {
    internalRectangle.getOrElse(null)
  }
  
  /**
   * Get specific interval count
   */
  def getIntervalCount(start: LocalDate) = {
    dates2ids.getOrElse(start, (0, Stream()))._1
  }
  
  def getCurrentCount(start: LocalDate) = {
    dates2ids.getOrElse(start, (0, Stream()))._1
  }
  
  /**
   * Get the maximum day count for a tag
   */
  def getMaxIntervalCount() = {
    dates2ids.maxBy { case (day, (count, ids)) => count }._2._1
  }
  
  def getDiscussionInLevel(start: LocalDate) = {
    dates2ids.getOrElse(start, (0, Stream()))._2.size
  }
  
  def getInterval2Ids(life: Life, date2step: Map[LocalDate, Int]) = {
    val newIds = dates2ids.par.groupBy {
      case (date, (count, ids)) =>
        val index = date2step.get(date).getOrElse(0) // retrieve actual step index respect to the interval
        life.incrementByInterval(index)
    }.mapValues { values =>
      val newCount = values.map { case (date, (count, ids)) => count }.sum
      val newIds = values.flatMap { case (date, (count, ids)) => ids }.toStream
      (newCount, newIds)
    }.toMap.seq
    
    dates2ids = newIds
  }
  
  def getLocationCenter() = {
    val internal = internalRectangle.getOrElse(new ScalaRectangle(0, 0, 0, 0))
    internal.getCenter()
  }
}
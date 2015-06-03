package visualization

import multidimensionalscaling.Document
import java.util.Date
import org.joda.time.LocalDate
import database.Tag

case class Location(val tags: List[String], val total: Double, val currentCount: Double, val dates2ids: Map[LocalDate, (Int, Stream[Int])], 
    val rectangle: Option[ScalaRectangle], val internalRectangle: Option[ScalaRectangle], var selected: Boolean) {

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
}
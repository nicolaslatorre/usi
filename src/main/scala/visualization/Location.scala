package visualization

import multidimensionalscaling.Document
import java.util.Date
import org.joda.time.LocalDate
import database.Tag

case class Location(val tag: Tag, val count: Double, val rectangle: Option[ScalaRectangle], val internalRectangle: Option[ScalaRectangle], var selected: Boolean) {
  
  /**
   * Get a list of tags
   */
  def getTagsAsList() = {
    tag.tags
  }
  
  /**
   * Get tags as a string
   */
  def getTagsAsString() = {
    tag.getTagsAsString()
  }
  
  def getRectangle() = {
    rectangle.getOrElse(null)
  }
  
  def getInternalRectangle() = {
    internalRectangle.getOrElse(null)
  }
  
  def getTotalCount() = {
    tag.totalCount
  }
  
  def getDates2Counts() = {
    tag.days2counts
  }
  
  def getIds() = {
    tag.dates2ids
  }

}
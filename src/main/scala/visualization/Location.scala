package visualization

import multidimensionalscaling.Document
import java.util.Date
import org.joda.time.LocalDate

case class Location(val tags: String, val ids: Map[LocalDate, Int], val count: Double, val rectangle: Rectangle, var selected: Boolean, val totalCount: Int) {
  
  /**
   * Get a list of tags
   */
  def getTagsAsList() = {
    tags.split(" ").toList
  }

}
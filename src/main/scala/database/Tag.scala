package database

import java.util.Date

import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks

import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._

class Tag(val tags: List[String], val ids: Map[LocalDate, Int], val count: Int, val interval: Int, val counts: Map[LocalDate, Int]) {

  /**
   * Get a tag list as a string
   */
  def getTagsAsString() = {
    tags.mkString(" ")
  }

  /**
   * Check if there are discussions in the given interval of dates
   * @param startDate the start date
   * @param endDate the end date
   * @return The ids and date in the given interval, returns an empty Map if there no ids in the given interval
   */
  def getIdsInDate(startDate: LocalDate, endDate: LocalDate) = {
    ids.filter {
      case (date, id) =>
        date >= startDate && date < endDate
    }
  }
  
  /**
   * Get specific count
   */
  def getCount(start: LocalDate) = {
    counts.getOrElse(start, 0)
  }

  def getMaxCount() = {
    counts.maxBy { case (month, count) => count }._2
  }

}
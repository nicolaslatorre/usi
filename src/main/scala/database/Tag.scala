package database

import java.util.Date

import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks

import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._

class Tag(val tags: List[String], val totalCount: Int, val dates2counts: Map[LocalDate, Int]) {

  /**
   * Get a tag list as a string
   */
  def getTagsAsString() = {
    tags.mkString(" ")
  }
  
  /**
   * Get specific count
   */
  def getCount(start: LocalDate) = {
    dates2counts.getOrElse(start, 0)
  }

  /**
   *  Get max count
   */
  def getMaxIntervalCount() = {
    dates2counts.maxBy { case (month, count) => count }._2
  }

}
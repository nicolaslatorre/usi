package database

import java.util.Date
import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks
import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._
import visualization.Life

class Tag(val tags: List[String], val totalCount: Int, var dates2counts: Map[LocalDate, Int], val days2counts: List[(Int, Int)], val ids: Map[LocalDate, Set[Int]], 
    var dates2ids: Map[LocalDate, Set[Int]]) {

  /**
   * Get a tag list as a string
   */
  def getTagsAsString() = {
    tags.mkString(" ")
  }

  def getTagSize() = {
    tags.size
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

  def changeDates2Counts(life: Life, date2step: Map[Int, Int]) = {
    val counts = days2counts.map {
      case (step, count) =>
        val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
        index -> count
    }.groupBy { case (step, count) => step }.mapValues { lineCounts =>
      lineCounts.map { case (step, count) => count }.sum
    }
    
    val d2c = counts.map {
      case (step, count) =>
        life.incrementByInterval(step) -> count
    }.toMap
    
    dates2counts = d2c
  }

}
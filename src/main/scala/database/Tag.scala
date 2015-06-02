package database

import java.util.Date
import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks
import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._
import visualization.Life

case class Tag(val tags: List[String], var totalCount: Int, val days2counts: List[(LocalDate, Int)], val ids: Map[LocalDate, Stream[Int]],
  var dates2ids: Map[LocalDate, Stream[Int]]) {

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
    days2counts.toMap.getOrElse(start, 0)
  }

  /**
   *  Get max count
   */
  def getMaxIntervalCount() = {
    days2counts.maxBy { case (month, count) => count }._2
  }

  def changeDates2Counts(life: Life, date2step: Map[LocalDate, Int]) = {
//    val d2c = days2counts.groupBy {
//      case (step, count) =>
//        val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
//        life.incrementByInterval(index)
//    }.mapValues { lineCounts =>
//      lineCounts.map { case (step, count) => count }.sum
//    }.toMap
//
//    val d2i = ids.groupBy {
//      case (date, count) =>
//        val index = date2step.get(date).getOrElse(0) // retrieve actual step index respect to the interval
//        life.incrementByInterval(index)
//    }.mapValues { values =>
//      values.flatMap { case(date, id) => id }.toStream
//    }.toMap

//    dates2counts = dates
//    dates2ids = d2i
  }

}
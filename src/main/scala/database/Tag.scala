package database

import java.util.Date
import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks
import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._
import visualization.Life

case class Tag(val tags: List[String], val total: Int, val days2ids: Map[LocalDate, (Int, Stream[Int])]) {

  /**
   * Get a tag list as a string
   */
  def getTagsAsString() = {
    tags.mkString(" ")
  }

  /**
   * Return the number of tags
   */
  def getTagSize() = {
    tags.size
  }

  /**
   * Get specific day count
   */
  def getDayCount(start: LocalDate) = {
    days2ids.getOrElse(start, (0, Stream()))._1
  }

  /**
   * Get the maximum day count for a tag
   */
  def getMaxDayCount() = {
    days2ids.maxBy { case (day, (count, ids)) => count }._2._1
  }

//  def getInterval2Ids(life: Life, date2step: Map[LocalDate, Int]) = {
//    days2ids.par.groupBy {
//      case (date, (count, ids)) =>
//        val index = date2step.get(date).getOrElse(0) // retrieve actual step index respect to the interval
//        life.incrementByInterval(index)
//    }.mapValues { values =>
//      val newCount = values.map { case (date, (count, ids)) => count }.sum
//      val newIds = values.flatMap { case (date, (count, ids)) => ids }.toStream
//      (newCount, newIds)
//    }.toMap.seq
//  }

}
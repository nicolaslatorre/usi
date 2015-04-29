package database

import java.util.Date

import org.joda.time.Interval
import org.joda.time.Months
import org.joda.time.Weeks

import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._

class Tag(val tags: List[String], val ids: Map[LocalDate, Int], val count: Int, val interval: Int, val counts: Map[LocalDate, Int]) {
//  val counts = getMonthsCount(new LocalDate(2008, 7, 31).withDayOfMonth(1), new LocalDate(2015, 3, 28).withDayOfMonth(1), interval)

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
   * Generate a map between the month number and the count of that month.
   */
  def getMonthsCount(start: LocalDate, end: LocalDate, interval: Int) = {
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths)

    ms.filter{ month => month % interval == 0}.map { month => getSingleMonthCount(start.plusMonths(month)) }.toMap
    //    ms.map { month => start.plusMonths(month) -> getAverageMonthCount(start.plusMonths(month)) }.toMap
  }
  
  /**
   * Get specific count
   */
  def getCount(start: LocalDate) = {
    counts.getOrElse(start, 0)
  }

  def getAverageMonthCount(start: LocalDate) = {
    val end = start.plusMonths(1)
    val interval = new Interval(start.toDate().getTime, end.toDate().getTime)
    val duration = interval.toDuration().getStandardDays.toInt

    val days = (0 until duration).toList

    val idsInInterval = ids.filter {
      case (date, id) =>
        date >= start && date < end
    }

    val counts = idsInInterval.groupBy { case (id, date) => new LocalDate(date).getDayOfMonth }.mapValues { values => values.size }
    (counts.map { case (id, count) => count }.sum / duration.toDouble)
  }

  def getSingleMonthCount(start: LocalDate) = {
    val end = start.plusMonths(1)
    val counts = getIdsInDate(start, end)
    start -> counts.values.size
    //start -> counts.size // the size can be computed on demand, here it can be useful to have the counts
  }

  def getMaxCount() = {
    counts.maxBy { case (month, count) => count }._2
  }

}
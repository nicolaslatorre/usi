package database

import java.util.Date

import org.joda.time.Interval
import org.joda.time.Months

import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports._

class Tag(val tags: List[String], val ids: Map[Int, Date], val count: Int) {
	val inverseIds = ids.groupBy{ case(id, date) => date }.mapValues{ map => map.keySet}
  val counts = getMonthsCount(new LocalDate(2008, 7, 31).withDayOfMonth(1), new LocalDate(2015, 3, 28).withDayOfMonth(1))

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
  def getIdsInInterval(startDate: LocalDate, endDate: LocalDate) = {
    inverseIds.filter {
      case (date, id) =>
        (date.toLocalDate >= startDate && date.toLocalDate < endDate)
    }
  }
  
  def getIdsInDate(startDate: LocalDate, endDate: LocalDate) = {
    ids.filter {
      case (id, date) =>
        date.toLocalDate >= startDate && date.toLocalDate < endDate
    }
  }

  /**
   * Generate a map between the month number and the count of that month.
   */
  def getMonthsCount(start: LocalDate, end: LocalDate) = {
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths)

    ms.map { month => getSingleMonthCount(start.plusMonths(month)) }.toMap
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
      case (id, date) =>
        date.getTime >= start.toDate().getTime && date.getTime < end.toDate().getTime
    }

    val counts = idsInInterval.groupBy { case (id, date) => new LocalDate(date).getDayOfMonth }.mapValues { values => values.size }
    (counts.map { case (id, count) => count }.sum / duration.toDouble)
  }

  def getSingleMonthCount(start: LocalDate) = {
    val end = start.plusMonths(1)
    val counts = getIdsInInterval(start, end)
    start -> counts.flatMap{ case(date, id) => id}.toList.size
    //start -> counts.size // the size can be computed on demand, here it can be useful to have the counts
  }

  def getMaxCount() = {
    counts.maxBy { case (month, count) => count }._2
  }

}
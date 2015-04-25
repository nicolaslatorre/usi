package database

import java.util.Date
import com.github.nscala_time.time.Imports.LocalDate
import org.joda.time.Months
import org.joda.time.Interval

class Tag(val tags: List[String], val ids: Map[Int, Date], val count: Int) {
  val counts = getMonthsCount(new LocalDate(2008, 7, 31).withDayOfMonth(1), new LocalDate(2015, 3, 28).withDayOfMonth(1))

  /**
   * Generate a map between the month number and the count of that month.
   */
  def getMonthsCount(start: LocalDate, end: LocalDate) = {
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths)

    ms.map { month => getSingleMonthCount(start.plusMonths(month)) }.toMap
//    ms.map { month => start.plusMonths(month) -> getAverageMonthCount(start.plusMonths(month)) }.toMap
  }

  def getAverageMonthCount(start: LocalDate) = {
    val end = start.plusMonths(1)
    val interval = new Interval(start.toDate().getTime, end.toDate().getTime)
    val duration = interval.toDuration().getStandardDays.toInt

    val days = (0 until duration).toList

    val idsInInterval = ids.filter { case (id, date) => 
        date.getTime >= start.toDate().getTime && date.getTime < end.toDate().getTime
    }
    
    val counts = idsInInterval.groupBy{ case(id, date) => new LocalDate(date).getDayOfMonth}.mapValues{ values => values.size}
    (counts.map{ case(id, count) => count}.sum / duration.toDouble)
  }

  def getSingleMonthCount(start: LocalDate) = {
    val end = start.plusMonths(1)

    val counts = ids.filter {
      case (id, date) =>
        date.getTime >= start.toDate.getTime && date.getTime < end.toDate().getTime
    }

    start -> counts.size
  }

  def getMaxCount() = {
    counts.maxBy { case (month, count) => count }._2
  }

}
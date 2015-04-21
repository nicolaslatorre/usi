package database

import java.util.Date
import com.github.nscala_time.time.Imports.LocalDate
import org.joda.time.Months

class Tag(val tags: List[String], val ids: Map[Int, Date], val count: Int) {
  val counts = getMonthsCount(new LocalDate(2008, 7, 31), new LocalDate(2015, 3, 8))
  
  
  /**
   * Generate a map between the month number and the count of that month.
   */
  def getMonthsCount(start: LocalDate, end: LocalDate) = {
    val months = Months.monthsBetween(start, end)
    
    val ms = (0 to months.getMonths)
    
    ms.par.map{ month => getSingleMonthCount(start.plusMonths(month), month)}.toMap 
  }
  

  def getSingleMonthCount(start: LocalDate, monthNumber: Int) = {
    val end = start.plusMonths(1)

    val counts = ids.filter {
      case(id, date) =>
        date.getTime >= start.toDate.getTime && date.getTime < end.toDate().getTime
    }
    
    monthNumber -> counts.size
  }
  
  def getMaxCount() = {
    counts.maxBy{ case(month, count) => count}._2
  }

}
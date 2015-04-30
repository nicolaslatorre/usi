package visualization

import org.joda.time.LocalDate
import org.joda.time.Months
import org.joda.time.Weeks
import org.joda.time.Days

class Life(val start: LocalDate, val end: LocalDate, val interval: Int) {

  def increment(value: Int) = {
    start.plusDays(value)
  }
  
  def incrementDate(date: LocalDate) = {
    date.plusDays(interval)
  }

  def days = {
    val ds = Days.daysBetween(start, end)
    val nrSteps = ds.getDays
    (0 to nrSteps).toStream
  }

  /**
   * Mapping
   */
  def getStepsMapping() = {
    val steps = days
    val monthSteps = steps.grouped(interval).toList

    monthSteps.zipWithIndex.map { case(steps, index) =>
      steps.map {
        step =>
          val date = increment(step)
          date -> index
      }.toMap
    }.foldLeft(Map[LocalDate, Int]())((m1, m2) => m1 ++ m2)
  }

}
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
  
  def dates = {
    val steps = days
    val intervalSteps = steps.grouped(interval).toList
    
    intervalSteps.zipWithIndex.map { case(step, index) => 
      start.plusDays(index*interval)  
    }
  }

  /**
   * Mapping between a date and the index of a step
   */
  def getStepsMapping() = {
    val steps = days
    val intervalSteps = steps.grouped(interval)

    intervalSteps.zipWithIndex.map { case(steps, index) =>
      steps.par.map {
        step =>
          val date = increment(step)
          date -> index
      }.toMap
    }.foldLeft(Map[LocalDate, Int]())((m1, m2) => m1 ++ m2)
  }

}
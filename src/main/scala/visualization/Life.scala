package visualization

import com.github.nscala_time.time.Imports._
import org.joda.time.Months
import org.joda.time.Weeks
import org.joda.time.Days


class Life(val start: LocalDate, val end: LocalDate, var interval: Int) {

  def increment(value: Int) = {
    start.plusDays(value)
  }
  
  def incrementDate(date: LocalDate) = {
    date.plusDays(interval)
  }
  
  def incrementByInterval(value: Int) = {
    increment(value * interval)
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
  
  def steps = {
    val steps = days
    val intervalSteps = steps.grouped(interval).toList
    intervalSteps.zipWithIndex.map{ case(ste, index) => index}
  }
  
  /**
   * Mapping between a step and the index of a step
   */
  def getDateMapping() = {
    val steps = days
    val intervalSteps = steps.grouped(interval)

    intervalSteps.zipWithIndex.flatMap { case(steps, index) =>
      steps.par.map {
        step =>
          increment(step) -> index
      }.toMap
    }.toMap
  }
}
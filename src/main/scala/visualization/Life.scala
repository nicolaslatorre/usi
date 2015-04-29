package visualization

import org.joda.time.LocalDate
import org.joda.time.Months
import org.joda.time.Weeks
import org.joda.time.Days



class Life(val start: LocalDate, val end: LocalDate) {
  type IncrementStrategy = Int => LocalDate
  type StepStrategy = Stream[Int]
  
  def increment(strategy: IncrementStrategy, value: Int) = {
    strategy(value)
  }
  
  def incrementByMonth: IncrementStrategy = {
    start.plusMonths(_)
  }
  
  def incrementByWeek: IncrementStrategy = {
    start.plusWeeks(_)
  }
  
  def incrementByDay: IncrementStrategy = {
    start.plusDays(_)
  }
  
  
  
  

  def getSteps(strategy: StepStrategy) = {
    strategy
  }
  
  def months: StepStrategy = {
    val ms = Months.monthsBetween(start, end)
    val nrSteps = ms.getMonths
    (0 to nrSteps).toStream
  }
  
  def weeks: StepStrategy = {
    val ws = Weeks.weeksBetween(start, end)
    val nrSteps = ws.getWeeks
    (0 to nrSteps).toStream
  }
  
  def days: StepStrategy = {
    val ds = Days.daysBetween(start, end)
    val nrSteps = ds.getDays
    (0 to nrSteps).toStream
  }
  

}
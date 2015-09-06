package visualization

import java.awt.Color
import java.text.SimpleDateFormat
import org.jfree.chart.ChartFactory
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.DateAxis
import org.jfree.chart.axis.DateTickUnit
import org.jfree.chart.axis.DateTickUnitType
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.ClusteredXYBarRenderer
import org.jfree.data.time._
import org.jfree.ui.RectangleInsets
import com.github.nscala_time.time.Imports._
import scalax.chart.module.ChartFactories
import scalax.chart.module.Charting
import scalax.chart.module.Charting._
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.renderer.xy.XYBarRenderer

object Graph {

  def drawBarChartGraph(locations: List[Location], life: Life) = {
    val dates = life.dates
    val dataset = new org.jfree.data.time.TimeSeriesCollection
    
    val days = dates.map{ date => 
      (new Day(date.toDate()), date)
    }
    
    locations.foreach { location =>
      val s = new org.jfree.data.time.TimeSeries(location.getTagsAsString())
      days.foreach { case(day, date) =>
        val value = location.getIntervalCount(date)
        s.add(day, value)
      }
      dataset.addSeries(s)
    }

    val renderer = new ClusteredXYBarRenderer();
    renderer.setShadowVisible(false)
    
    val xyPlot = new XYPlot(dataset, new DateAxis("Dates"), new NumberAxis("Discussions"), renderer);
    xyPlot.setOrientation(PlotOrientation.VERTICAL);

    //    val chart = ChartFactory.createXYBarChart("Occurrences", "Dates", true, "Discussions", dataset)
    val chart = new JFreeChart("Occurrences", JFreeChart.DEFAULT_TITLE_FONT, xyPlot, true)

    // Set chart styles
    chart.setBackgroundPaint(Color.white);

    // Set plot styles
    val plot = chart.getPlot().asInstanceOf[XYPlot]
    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinePaint(Color.white)
    plot.setRangeGridlinePaint(Color.white)
    plot.getDomainAxis.setVerticalTickLabels(true)
    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0))

    // Set date axis style
    val axis = plot.getDomainAxis().asInstanceOf[DateAxis]
    val formatter = new SimpleDateFormat("MMM yyyy")

    axis.setMinimumDate(life.start.minusMonths(1).toDate)
    axis.setMaximumDate(life.end.plusMonths(2).toDate)
    val unit = new DateTickUnit(DateTickUnitType.MONTH, 2, formatter)
    axis.setTickUnit(unit)

    chart
  }

  def drawAreaChartGraph(locations: List[Location], life: Life) = {
    val dates = life.dates
    val dataset = new org.jfree.data.time.TimeTableXYDataset

    locations.foreach { location =>
      val series = location.getTagsAsString()
      dates.foreach { date =>
        val value = location.getIntervalCount(date)
        val period = new Day(date.getDayOfMonth, date.getMonthOfYear, date.getYear)
        dataset.add(period, value, series)
      }

    }

    val chart = ChartFactory.createStackedXYAreaChart("Occurrences", "Dates", "Discussions", dataset, PlotOrientation.VERTICAL, true, true, false)

    // Set chart styles
    chart.setBackgroundPaint(Color.white);

    // Set plot styles
    val plot = chart.getXYPlot
    plot.setBackgroundPaint(Color.lightGray)
    plot.setDomainGridlinePaint(Color.white)
    plot.setRangeGridlinePaint(Color.white)
    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0))
    
    val range = plot.getDomainAxis.getRange
    
    plot.setDomainAxes(Array(new DateAxis("Dates")))
    plot.getDomainAxis.setVerticalTickLabels(true)

    plot.setRangeAxis(new NumberAxis("Discussions"))

    // Set date axis style
    val axis = plot.getDomainAxis().asInstanceOf[DateAxis]
    val formatter = new SimpleDateFormat("MMM yyyy")

    axis.setMinimumDate(life.start.minusMonths(1).toDate)
    axis.setMaximumDate(life.end.plusMonths(2).toDate)
    
    val unit = new DateTickUnit(DateTickUnitType.MONTH, 2, formatter)
    axis.setTickUnit(unit)
    axis.setRange(range, false, true)


    chart
  }

  def drawLineChartGraph(locations: List[Location], life: Life) = {

    val dates = life.dates
    val dataset = new org.jfree.data.time.TimeSeriesCollection

    locations.foreach { location =>
      val s = new org.jfree.data.time.TimeSeries(location.getTagsAsString())
      dates.foreach { date =>
        val value = location.getIntervalCount(date)
        s.add(new Day(date.toDate()), value)
      }
      dataset.addSeries(s)
    }

    val chart = ChartFactory.createTimeSeriesChart("Occurrences", "Dates", "Discussions", dataset)

    val plot = chart.getXYPlot
    val range = plot.getDomainAxis.getRange
    plot.setDomainAxis(new DateAxis("Dates"))
    plot.setRangeAxis(new NumberAxis("Discussions"))
    plot.getDomainAxis.setVerticalTickLabels(true)

    val axis = plot.getDomainAxis().asInstanceOf[DateAxis]
    val formatter = new SimpleDateFormat("MMM yyyy")
    val unit = new DateTickUnit(DateTickUnitType.MONTH, 2, formatter)
    axis.setTickUnit(unit)
    axis.setRange(range, false, true)
    
    chart
  }
}
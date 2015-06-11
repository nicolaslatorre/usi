package visualization

import org.jfree.chart.ChartFactory
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.ClusteredXYBarRenderer
import org.jfree.chart.renderer.xy.XYBarPainter
import org.jfree.data.time._
import com.github.nscala_time.time.Imports._
import scalax.chart.module.ChartFactories
import scalax.chart.module.Charting
import scalax.chart.module.Charting._
import org.jfree.chart.renderer.xy.StandardXYBarPainter
import org.jfree.chart.renderer.xy.XYAreaRenderer

object Graph {
  def main(args: Array[String]) = {

    val ds = new org.jfree.data.category.DefaultCategoryDataset
    ds.addValue(5, "Class A", "Bin 1")
    ds.addValue(7, "Class A", "Bin 2")
    ds.addValue(8, "Class B", "Bin 1")
    ds.addValue(3, "Class B", "Bin 2")

    val chart = ChartFactories.LineChart(ds)
    //    chart.peer.getPlot.
    chart.show()

    val chartLine = XYLineChart(List((1, 2), (2, 7), (3, 5)))
    chartLine.show()
  }

  def drawBarChartGraph(locations: List[Location], life: Life) = {
    val dates = life.dates
    val dataset = new XYSeriesCollection

    locations.foreach { location =>
      val s = new XYSeries(location.getTagsAsString())
      dates.zipWithIndex.foreach {
        case (date, index) =>
          val value = location.getIntervalCount(date)
          s.add(index, value)
      }
      dataset.addSeries(s)
    }

    val renderer = new ClusteredXYBarRenderer()
    renderer.setShadowVisible(false)
    renderer.setBarPainter(new StandardXYBarPainter())

    val plot = new XYPlot(dataset, new NumberAxis("Dates"), new NumberAxis("Discussions"), renderer)
    plot.setOrientation(PlotOrientation.VERTICAL)

    //    val chart = ChartFactory.createXYBarChart("Occurrences", "Date", true, "Frequency", dataset.to)
    val chart = new JFreeChart("Occurrences", JFreeChart.DEFAULT_TITLE_FONT, plot, true);
    chart
  }

  def drawAreaChartGraph(locations: List[Location], life: Life) = {

    val dates = life.dates
    val dataset = new XYSeriesCollection

    locations.sortBy{ location => location.dates2ids.maxBy { case (day, (count, ids)) => count }._2._1}.foreach { location =>
      val s = new XYSeries(location.getTagsAsString())
      dates.zipWithIndex.foreach {
        case (date, index) =>
          val value = location.getIntervalCount(date)
          s.add(index, value)
      }
      dataset.addSeries(s)
    }

    val renderer = new XYAreaRenderer()

    val plot = new XYPlot(dataset, new NumberAxis("Dates"), new NumberAxis("Discussions"), renderer)
    plot.setOrientation(PlotOrientation.VERTICAL)

    //    val chart = ChartFactory.createXYBarChart("Occurrences", "Date", true, "Frequency", dataset.to)
    val chart = new JFreeChart("Occurrences", JFreeChart.DEFAULT_TITLE_FONT, plot, true);
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
    chart.getXYPlot.getDomainAxis.setVerticalTickLabels(true)
    chart
  }
}
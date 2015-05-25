package visualization

import org.jfree.chart.ChartFactory
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.category.StandardBarPainter
import org.jfree.chart.renderer.xy.ClusteredXYBarRenderer
import org.jfree.data.time._
import com.github.nscala_time.time.Imports._
import database.Tag
import scalax.chart.module.ChartFactories
import scalax.chart.module.Charting
import scalax.chart.module.Charting._
import org.jfree.chart.renderer.xy.XYBarPainter

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

  def drawBarCharGraph(tags: List[Location], life: Life) = {
    val dates = life.dates
    val dataset = new XYSeriesCollection
    
    tags.foreach{ tag => 
      val s = new XYSeries(tag.getTagsAsString())
      dates.zipWithIndex.foreach { case(date, index) => 
        val value = tag.getDates2Counts().get(date).getOrElse(0)
        s.add(index, value)
      }
      dataset.addSeries(s)
    }
    
    val renderer = new ClusteredXYBarRenderer()
    
    val plot = new XYPlot(dataset, new NumberAxis("Dates"), new NumberAxis("Discussions"), renderer)
    plot.setOrientation(PlotOrientation.VERTICAL)

//    val chart = ChartFactory.createXYBarChart("Occurrences", "Date", true, "Frequency", dataset.to)
    val chart = new JFreeChart("Occurrences", JFreeChart.DEFAULT_TITLE_FONT, plot, true);
    chart
  }

  def drawLineCharGraph(tags: List[Location], life: Life) = {

    val dates = life.dates
    val dataset = new org.jfree.data.time.TimeSeriesCollection


    tags.foreach{ tag => 
      val s = new org.jfree.data.time.TimeSeries(tag.getTagsAsString())
      dates.foreach { date => 
        val value = tag.getDates2Counts().get(date).getOrElse(0)
        s.add(new Day(date.toDate()), value)
      }
      dataset.addSeries(s)
    }

    val chart = ChartFactory.createTimeSeriesChart("Occurrences", "Dates", "Discussions", dataset)
    chart.getXYPlot.getDomainAxis.setVerticalTickLabels(true)
    chart
  }
}
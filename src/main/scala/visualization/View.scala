package visualization

import java.awt.Color
import java.awt.Dimension
import java.awt.Toolkit
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import java.awt.geom.Rectangle2D
import java.io.File
import java.util.ArrayList
import scala.collection.JavaConversions._
import scala.swing.Action
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position._
import scala.swing.BoxPanel
import scala.swing.EditorPane
import scala.swing.FileChooser
import scala.swing.FileChooser.Result
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Label
import scala.swing.Menu
import scala.swing.MenuBar
import scala.swing.MenuItem
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.TextArea
import scala.swing.TextField
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import multidimensionalscaling.Document
import scala.swing.Slider
import scala.swing.Button
import com.github.nscala_time.time.Imports._
import java.util.Date
import org.joda.time.Instant

object Starter {
  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val levels = 15

    val model = new Model(url, username, password, levels, new LocalDate(2008, 8, 31))

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val view = new View(model, levels)
        val control = new Control(model, view)
        control.view.peer.setVisible(true)
      }
    })

  }
}

class View(val model: Model, val levels: Int) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  val panel = new BorderPanel {
    val canvas = new Canvas(model)
    val scrollPane = new ScrollPane() {
      contents = canvas
    }

    val menuEast = new FlowPanel() {
      preferredSize = new Dimension(1440, 30)
      val label = new Label("Current tag: ")
      val text = new Label("Stack Overflow")
      val labelTag = new Label("\tOccurrences: ")
      val occurrences = new Label(model.tree.root.tag.count.toString)
      contents += label
      contents += text
      contents += labelTag
      contents += occurrences
    }

    val sliderPanel = new FlowPanel() {
    	preferredSize = new Dimension(1440, 60)
      val slider = new Slider() {
        preferredSize = new Dimension(1440, 30)
        val start = new LocalDate(2008, 7, 31)
        val end = new LocalDate(2015, 3, 8)
        val interval = new Interval(start.toDate().getTime, end.toDate().getTime)
        
        
        min = 0 //2008-7-31
        max = interval.toDuration().getStandardDays.toInt// 2015-3-8
        
        labels = Map(min -> new Label("0"), max -> new Label(max.toString()))
        paintLabels = true
        
        println("Max: " + max)
        val valueDate = start.plusDays(30)
        value = new Interval(start.toDate.getTime, valueDate.toDate().getTime).toDuration().getStandardDays.toInt
        majorTickSpacing = 1 // one day
      }

      val playButton = new Button {
        text = "Play"
      }
      
      val resetButton = new Button {
        text = "Reset"
      }

      contents += slider
      contents += playButton
      contents += resetButton
    }

    layout(scrollPane) = Center
    layout(menuEast) = North
    layout(sliderPanel) = South
  }
  contents = panel
  pack
}

class Canvas(val model: Model) extends Panel {
  requestFocus()
  preferredSize = Toolkit.getDefaultToolkit.getScreenSize
  println(preferredSize.getWidth + ", " + preferredSize.getHeight)
  val backgroundColor = Color.WHITE
  opaque = true
  background = backgroundColor

  var locations = model.locations

  var zoomFactor = 0.1
  var offsetX = 0.0
  var offsetY = 0.0

  var drawMessages = false
  var drawAllMessages = false

  var changingViewPort = false
  var viewPortX = 0
  var viewPortY = 0
  var viewPortWidth = 0
  var viewPortHeight = 0

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    val gradient = model.gradient
    println("Total location: " + locations.size)

    val size = Toolkit.getDefaultToolkit.getScreenSize
    val currentNodeChildrens = locations.tail

    currentNodeChildrens.foreach { location =>
      val rect = location.rect
      if (rect != null) {
        g.setColor(Color.BLACK)
        g.draw(new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height))
        g.setColor(getColor(location.ids, model.startDate))
        g.fill(new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height))
        g.setColor(Color.BLACK)

        val tagIndex = location.tags.lastIndexOf(" ")
        val message = {
          if (tagIndex == -1) location.tags
          else location.tags.substring(tagIndex, location.tags.length)
        }
        if (rect.width >= 75) g.drawString(message.toString, rect.x, rect.y + rect.height / 2)
      } else {
        println(location.tags + " is null with occurrences: " + location.count)
      }
    }
    println("Drew squares")

    if (changingViewPort) {
      g.setColor(Color.BLACK)
      g.drawRect(viewPortX, viewPortY, viewPortWidth, viewPortHeight)
    }
  }

  //  def getColor(rect: Rectangle) = {
  //    rect.width match {
  //      case 90 => new Color(255, 0, 0)
  //      case 75 => new Color(255, 60, 60)
  //      case 60 => new Color(255, 120, 120)
  //      case 45 => new Color(255, 180, 180)
  //      case 30 => new Color(255, 210, 210)
  //      case 15 => new Color(255, 230, 230)
  //    }
  //  }

  def getColor(ids: Map[Int, Date], date: LocalDate) = {
	  val initial = date.minusMonths(1) 
    val inInterval = ids.filter{case(id, d) => 
      d.toLocalDate >= initial && d.toLocalDate < date
    }

    val colorStart = new Color(0, 0, 255)
    val colorEnd = new Color(255, 0, 0)
    val levels = 30

    val ls = (0 to levels)

    var red1 = colorStart.getRed
    var blue1 = colorStart.getBlue

    val red2 = colorEnd.getRed
    val blue2 = colorEnd.getBlue

    val stepRed = Math.abs(red1 - red2) / levels
    val stepBlue = Math.abs(blue1 - blue2) / levels

    val gradient = ls.map { x =>
      x match {
        case 0 => (0, colorStart)
        case x =>
          if (x == levels) (levels, colorEnd)
          else {

            val newRed = updateColor(red1, red2, stepRed)
            val newBlue = updateColor(blue1, blue2, stepBlue)

            red1 = newRed
            blue1 = newBlue

            (x, new Color(newRed, 0, newBlue))
          }
      }
    }.toMap
    
    val maxDate = inInterval.toList.maxBy{ case(id, date) => date}._2.toLocalDate
    
    val interval = new Interval(initial.toDate.getTime, maxDate.toDate.getTime)
    val day = interval.toDuration().getStandardDays.toInt
    
    println("Da day: " + day)
    
    gradient.get(day).get
    
  }

  def updateColor(start: Int, end: Int, step: Int) = {
    if (start > end) start - step
    else start + step
  }
}
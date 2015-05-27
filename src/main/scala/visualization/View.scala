package visualization

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Toolkit
import java.awt.geom.Rectangle2D

import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.East
import scala.swing.BorderPanel.Position.North
import scala.swing.BorderPanel.Position.South
import scala.swing.BorderPanel.Position.West
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Label
import scala.swing.ListView
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.Slider
import scala.swing.TextField

import com.github.nscala_time.time.Imports.Interval
import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports.richAbstractPartial
import com.github.nscala_time.time.Imports.richDate

import javax.swing.ImageIcon
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE

object Starter {
  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val startDate = new LocalDate(2008, 7, 31)
    val endDate = new LocalDate(2015, 3, 8)
    val interval = 7
    val life = new Life(startDate, endDate, interval)
    val name = "StackOverflow"

    val model = new Model(url, username, password, life, name)

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val view = new View(model)
        val control = new Control(model, view)
      }
    })

  }
}

class View(val model: Model) extends Frame {
  title = "StackOverflow Dataset Almanac"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)
  val life = model.life

  val mainPanel = new BorderPanel {
    val scrollPane = new ScrollPane() {
      val canvas = new Canvas(model)
      contents = canvas
    }

    val northPanel = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(1440, 110)

      val menuPanel = createMenuPanel()

      val homePanel = createHomePanel()

      contents += menuPanel
      contents += homePanel
    }

    val tagListPanel = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(200, 800)
      val list = new ListView(model.locations.map { location => location.getTagsAsString() }.sorted)
      val scrollPane = new ScrollPane() {
        contents = list
      }
      contents += scrollPane
      visible = false
    }

    val playerPanel = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(1440, 120)

      val slider = new Slider() {
        preferredSize = new Dimension(1440, 40)
        val life = model.life
        val steps = life.steps

        min = 0
        max = steps.size - 1

        val checkpoints = (steps.filter { step => step % 100 == 0 } :+ max).distinct
        labels = checkpoints.map { step => step -> new Label(step.toString) }.toMap
        paintLabels = true
        paintTicks = true

        value = min
        majorTickSpacing = 1 // one step
      }

      val playerButtonPanel = new BoxPanel(Orientation.Horizontal) {
        val startButton = createButtonWithImage("../Images/mono-player-start.png", 24, 24)
        val playButton = createButtonWithImage("../Images/mono-player-play.png", 24, 24)
        val stopButton = createButtonWithImage("../Images/mono-player-stop.png", 24, 24)
        val endButton = createButtonWithImage("../Images/mono-player-end.png", 24, 24)

        contents += startButton
        contents += playButton
        contents += stopButton
        contents += endButton
      }

      val datePanel = new FlowPanel(FlowPanel.Alignment.Center)() {
        val dateLabel = new Label {
          text = life.start.toString()
          font = new Font("Ariel", java.awt.Font.BOLD, 20)
        }
        contents += dateLabel
      }

      contents += slider
      contents += playerButtonPanel
      contents += datePanel
    }
    
    val discussionsPanel = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(200, 800)
//      val list = new ListView(model.locations.flatMap { location => location.ids.getOrElse(life.start, Set(0)) }.filter { elem => elem > 0 })
      val list = new ListView(List(""))
      val scrollPane = new ScrollPane() {
        contents = list
      }
      contents += scrollPane
      visible = false
    }

    layout(scrollPane) = Center
    layout(northPanel) = North
    layout(playerPanel) = South
    layout(tagListPanel) = East
    layout(discussionsPanel) = West
  }
  contents = mainPanel
  pack

  def createButtonWithImage(path: String, width: Int, height: Int) = {
    new Button {
      icon = new ImageIcon(new ImageIcon(path).getImage.getScaledInstance(width, height, java.awt.Image.SCALE_SMOOTH))
    }
  }

  def createMenuPanel() = {
    new FlowPanel(FlowPanel.Alignment.Left)() {
      val player = new Button("Player")
      val charts = new Button("Charts")
      val discussionsList = new Button("Discussions List")
      val tagList = new Button("Tag List")

      contents += player
      contents += charts
      contents += discussionsList
      contents += tagList
    }
  }

  def createHomePanel() = {
    new FlowPanel(FlowPanel.Alignment.Left)() {

      // MAIN PANEL
      val mainInfoPanel = new BoxPanel(Orientation.Vertical) {

        // Name Panel
        val namePanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val datasetNameLabel = new Label("Dataset: ")
          val datasetName = new Label(model.name)

          contents += datasetNameLabel
          contents += datasetName
        }

        // Path Panel
        val pathInfoPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val pathLabel = new Label("Current Tag Path: ")
          val path = new Label("Root")

          contents += pathLabel
          contents += path
        }

        // Add to Panel
        contents += namePanel
        contents += pathInfoPanel
      }

      // TOTAL PANEL
      val totalInfoPanel = new BoxPanel(Orientation.Vertical) {

        // Tags Panel
        val totalTagsPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val totalTagLabel = new Label("Total Number Of Tags: ")
          val totalTag = new Label() {
            val total = model.getTotalDataset()
            text = total.toString
          }

          contents += totalTagLabel
          contents += totalTag
        }

        // Discussion Panel
        val totalDiscussionsPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val totalDiscussionsLabel = new Label("Total Discussions: ")
          val totalDiscussions = new Label() {
            val total = model.getTotalOccurrences
            text = total.toString
          }

          contents += totalDiscussionsLabel
          contents += totalDiscussions
        }

        // Add to Panel
        contents += totalTagsPanel
        contents += totalDiscussionsPanel

      }

      // CURRENT PANEL
      val currentInfoPanel = new BoxPanel(Orientation.Vertical) {
        // Tags Panel
        val currentTagsPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val currentTagLabel = new Label("Tags in Interval: ")
          val currentTag = new Label() {
            val total = model.getTagNumberInInterval()
            text = total.toString
          }

          contents += currentTagLabel
          contents += currentTag
        }

        // Discussion Panel
        val currentDiscussionsPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
          val currentDiscussionsLabel = new Label("Discussions in Interval: ")
          val currentDiscussions = new Label() {
            val total = model.getCurrentTotalOccurences(life.start)
            text = total.toString
          }

          contents += currentDiscussionsLabel
          contents += currentDiscussions
        }

        // Add to Panel
        contents += currentTagsPanel
        contents += currentDiscussionsPanel
      }

      // NAVIGATION PANEL
      val navigationPanel = new BoxPanel(Orientation.Vertical) {

        val inspectButton = new Button("Inspect") {
          enabled = false
        }

        val clearButton = new Button("Clear")

        contents += inspectButton
        contents += clearButton
      }

      // CHART PANEL
      val chartsPanel = new BoxPanel(Orientation.Vertical) {
        val lineChartButton = new Button("Line Chart")
        val barChartButton = new Button("Bar Chart")

        contents += lineChartButton
        contents += barChartButton
        visible = false
      }
      
      // INTERVAL PANEL
      val loadingPanel = new BoxPanel(Orientation.Vertical) {
    	  val intervalPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
    		  val intervalLabel = new Label {
    			  text = "Interval length in days: "
    		  }
    		  
    		  val intervalValue = new TextField(life.interval.toString, 10)
    		  
    		  contents += intervalLabel
    		  contents += intervalValue
    	  }
        
//        val progressPanel = new FlowPanel(FlowPanel.Alignment.Left)() {
//          val progressLabel = new Label("Loaded: ")
//          val currentSize = model.mainVector.size
//          val progress = new Label() {
//        	  val percentages = (currentSize.toDouble / model.datasetSize) * 100
//            text = BigDecimal(percentages).setScale(2, BigDecimal.RoundingMode.HALF_UP).toString + "%"
//          }
//          
//          contents += progressLabel
//          contents += progress
//        }
        
        contents += intervalPanel
//        contents += progressPanel
        
      }

      contents += mainInfoPanel
      contents += totalInfoPanel
      contents += currentInfoPanel
      contents += navigationPanel
      contents += chartsPanel
      contents += loadingPanel

    }
  }
}

class Canvas(val model: Model) extends Panel {
  preferredSize = new Dimension(2000, 2000) //Toolkit.getDefaultToolkit.getScreenSize
  val backgroundColor = Color.WHITE
  opaque = true
  background = backgroundColor
  var zoomFactor = 1.0
  var offsetX = 0.0
  var offsetY = 0.0

  var locations = model.locations
  val gradient = model.gradient

  var changingViewPort = false
  var drawBorders = true

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)

    val size = Toolkit.getDefaultToolkit.getScreenSize
    val currentNodeChildrens = locations.tail

    currentNodeChildrens.filter { location => isInRectangle(location, location.getRectangle()) }.foreach { location =>
      val rectangle = location.getRectangle()
      val sub = location.getInternalRectangle()
      if (rectangle != null) {
        val offset = new Point(offsetX, offsetY)
        val pointExternal = (new Point(rectangle.x, rectangle.y) + offset) * zoomFactor

        if (drawBorders) {
          g.setColor(Color.BLACK)
          g.draw(new Rectangle2D.Double(pointExternal.x, pointExternal.y, rectangle.width * zoomFactor, rectangle.height * zoomFactor))
        }

        g.setColor(Color.BLACK)
        val pointInternal = (new Point(sub.x, sub.y) + offset) * zoomFactor
        g.draw(new Rectangle2D.Double(pointInternal.x, pointInternal.y, sub.width * zoomFactor, sub.height * zoomFactor))

        val key = (location.count / model.maxHeight.toDouble) * 30
        g.setColor(gradient.get(key.toInt).get)

        //        val toPaintRect = new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height)
        val toPaintRect = new Rectangle2D.Double(pointInternal.x, pointInternal.y, sub.width * zoomFactor, sub.height * zoomFactor)

        g.fill(toPaintRect)

        if (key > 15) g.setColor(Color.WHITE) else g.setColor(Color.BLACK)

        val tags = location.getTagsAsString()
        val tagIndex = tags.lastIndexOf(" ")
        val message = {
          if (tagIndex == -1) tags
          else tags.substring(tagIndex, tags.length)
        }
        if (rectangle.width >= 50) {
          val pointMessage = new Point(pointInternal.x, pointExternal.y) + new Point(0.0, (rectangle.height / 2) * zoomFactor)
          g.drawString(message.toString, pointMessage.x.toInt, pointMessage.y.toInt)
        }
        if (location.selected) {
          g.setColor(Color.RED)
          g.fillOval(pointExternal.x.toInt, pointExternal.y.toInt, 8, 8)
          g.draw(new Rectangle2D.Double(pointExternal.x, pointExternal.y, (rectangle.width - 1) * zoomFactor, (rectangle.height - 1) * zoomFactor))
        }
      } else {
        println(location.getTagsAsString() + " is null with occurrences: " + location.count)
      }
    }
    println("(View) Drew squares")
  }

  def isInRectangle(location: Location, rectangle: ScalaRectangle) = {
    val offset = new Point(offsetX, offsetY)
    val zoom = zoomFactor
    val size = Toolkit.getDefaultToolkit.getScreenSize

    val topLeft = (new Point(rectangle.x, rectangle.y) + offset) * zoom
    val bottomRight = (new Point(rectangle.x + rectangle.width, rectangle.y + rectangle.height) + offset) * zoom

    if (topLeft.x > size.getWidth || bottomRight.x < 0.0) false
    else if (topLeft.y > size.getHeight || bottomRight.y < 0.0) false
    else true
  }

}
package visualization

import java.awt.Color
import java.awt.Dimension
import java.awt.Toolkit
import java.awt.geom.Rectangle2D
import java.util.Date
import scala.swing.BorderPanel
import scala.swing.BorderPanel.Position.Center
import scala.swing.BorderPanel.Position.North
import scala.swing.BorderPanel.Position.South
import scala.swing.BorderPanel.Position.East
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.FlowPanel
import scala.swing.Frame
import scala.swing.Graphics2D
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.Slider
import com.github.nscala_time.time.Imports.Interval
import com.github.nscala_time.time.Imports.LocalDate
import com.github.nscala_time.time.Imports.richAbstractPartial
import com.github.nscala_time.time.Imports.richDate
import javax.swing.ImageIcon
import javax.swing.SwingUtilities
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import scala.swing.TextField
import org.joda.time.Months
import scala.swing.ListView

object Starter {
  def main(args: Array[String]) {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val startDate = new LocalDate(2008, 7, 31).withDayOfMonth(1)
    val endDate = new LocalDate(2015, 3, 8).withDayOfMonth(1)
    val interval = 1
    
    
    
//    val months = Months.monthsBetween(startDate, endDate)
//    println("(Starter) months: " + months.getMonths)
    
    val model = new Model(url, username, password, startDate, endDate, interval)

    SwingUtilities.invokeLater(new Runnable {
      def run {
        val view = new View(model)
        val control = new Control(model, view)
        control.view.peer.setVisible(true)
      }
    })

  }
}

class View(val model: Model) extends Frame {
  title = "StackOverflow Viewer"
  peer.setDefaultCloseOperation(EXIT_ON_CLOSE)

  val panel = new BorderPanel {
    val canvas = new Canvas(model)
    val scrollPane = new ScrollPane() {
      contents = canvas
    }

    val menuEast = new FlowPanel() {
      preferredSize = new Dimension(1440, 30)
      val label = new Label("Path: ")
      val text = new Label("Stack Overflow")
      val labelTag = new Label("\tOccurrences: ")
      val occurrences = new Label(model.tree.root.tag.count.toString)
      val showList = new Button("Show List")
      contents += label
      contents += text
      contents += labelTag
      contents += occurrences
      contents += showList
    }
    
    val selectionMenu = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(200, 800)
    	val number = new Label("Number of tags: " + model.tree.root.children.size.toString)
      contents += number
      val list = new ListView(model.locations.map { location => location.tags }.sorted)
      val scrollPane = new ScrollPane() {
        contents = list
      }
      contents += number
      contents += scrollPane
      
      visible = false
    }

    val sliderPanel = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(1440, 80)
      
      val slider = new Slider() {
        preferredSize = new Dimension(1440, 40)
        val life = new Life(model.startDate, model.endDate)
        
        val steps = life.getSteps(life.months)

        min = 0
        max = steps.size - 1
        
        val checkpoints = (steps.filter { month => month%10 == 0 } :+ max).distinct
        labels = checkpoints.map { step => step -> new Label(step.toString) }.toMap
        paintLabels = true
        paintTicks = true

        val valueDate = life.start
        value = model.months.getOrElse(valueDate, 0)
        majorTickSpacing = 1 // one day
      }

      val buttonPanel = new BoxPanel(Orientation.Horizontal) {
        val selectionButton = new Button {
          text = "Select"
          visible = false
        } 
        
        val startButton = new Button {
          icon = new ImageIcon(new ImageIcon("../Images/mono-player-start.png").getImage.getScaledInstance(24, 24, java.awt.Image.SCALE_SMOOTH))
        }

        val playButton = new Button {
          icon = new ImageIcon(new ImageIcon("../Images/mono-player-play.png").getImage.getScaledInstance(24, 24, java.awt.Image.SCALE_SMOOTH))
        }

        val stopButton = new Button {
          icon = new ImageIcon(new ImageIcon("../Images/mono-player-stop.png").getImage.getScaledInstance(24, 24, java.awt.Image.SCALE_SMOOTH))
        }

        val endButton = new Button {
          icon = new ImageIcon(new ImageIcon("../Images/mono-player-end.png").getImage.getScaledInstance(24, 24, java.awt.Image.SCALE_SMOOTH))
        }

        val dateLabel = new Label {
          text = slider.valueDate.toString()
        }

        val monthInterval = new FlowPanel {
          val monthLabel = new Label {
            text = "Window interval length in months: "
          }

          val monthValue = new TextField("1", 10)

          contents += monthLabel
          contents += monthValue
        }

        contents += selectionButton
        contents += startButton
        contents += playButton
        contents += stopButton
        contents += endButton
        contents += dateLabel
//        contents += monthInterval

      }

      contents += slider
      contents += buttonPanel
    }

    layout(scrollPane) = Center
    layout(menuEast) = North
    layout(sliderPanel) = South
    layout(selectionMenu) = East
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
  val gradient = model.gradient
  

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    println("(View) Total location: " + locations.size)

    val size = Toolkit.getDefaultToolkit.getScreenSize
    val currentNodeChildrens = locations.tail

    currentNodeChildrens.foreach { location =>
      val rect = location.rectangle
      if (rect != null && location.count > 0) {
        g.setColor(Color.BLACK)
        g.draw(new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height))
        val key = (location.count/model.maxHeight.toDouble) * 30
        g.setColor(gradient.get(key.toInt).get)
        
        val toPaintRect = new Rectangle2D.Double(rect.x, rect.y, rect.width, rect.height)
        
        g.fill(toPaintRect)
        
        if(key > 15) g.setColor(Color.WHITE) else g.setColor(Color.BLACK)
        

        val tagIndex = location.tags.lastIndexOf(" ")
        val message = {
          if (tagIndex == -1) location.tags
          else location.tags.substring(tagIndex, location.tags.length)
        }
        if (rect.width >= 75) g.drawString(message.toString, rect.x.toInt, rect.y.toInt + rect.height.toInt / 2)
        if(location.selected) {
          g.setColor(Color.RED)
          g.fillOval(rect.x.toInt, rect.y.toInt, 8, 8)
        }
      } else {
        println(location.tags + " is null with occurrences: " + location.count)
      }
    }
    println("(View) Drew squares")
  }
  
//  def createShapeComponents() = {
//    println("(Model) stanno costruendo...")
//    val childrens = locations.tail
//
//    childrens.flatMap { location =>
//      val rectangle = location.rectangle
//      if (rectangle != null) {
//        val key = (location.count/model.maxHeight.toDouble) * 30
//        val color = gradient.get(key.toInt).getOrElse(Color.WHITE)
//        val shape = new Rectangle2D.Double(rectangle.x, rectangle.y, rectangle.width, rectangle.height)
//        
//        
//        Some(new ShapeComponent(shape, color, location, key.toInt))
//      } else {
//        println(location.tags + " is null with occurrences: " + location.count)
//        None
//      }
//    }
//  }
  
}
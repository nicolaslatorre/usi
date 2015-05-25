package visualization

import scala.swing.event.MouseClicked
import scala.swing.event.MouseEvent
import com.github.nscala_time.time.Imports._
import scala.swing.event.ButtonClicked
import scala.swing.event.ValueChanged
import scala.swing.event.KeyReleased
import scala.swing.event.KeyPressed
import java.awt.Color
import scala.swing.event.Key
import scala.sys.process._
import scala.swing.event.SelectionChanged
import scala.swing.event.MouseWheelMoved
import scala.swing.event.MouseDragged
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased
import database.Node
import java.awt.Graphics2D
import org.jfree.chart.ChartPanel
import scala.swing.Frame
import java.awt.Dimension
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import org.jfree.chart.JFreeChart
import java.awt.geom.Rectangle2D

class Control(val model: Model, val view: View) {
  //PANELS
  val panel = view.mainPanel

  // Player Panel
  val playerPanel = panel.playerPanel
  val slider = playerPanel.slider
  val playerButtonsPanel = playerPanel.playerButtonPanel
  val datePanel = playerPanel.datePanel

  // North Panel
  val northPanel = panel.northPanel

  // Home Panel
  val homePanel = northPanel.homePanel
  val mainInfoPanel = homePanel.mainInfoPanel
  val currentInfoPanel = homePanel.currentInfoPanel
  val navigationPanel = homePanel.navigationPanel
  val chartsPanel = homePanel.chartsPanel

  // Menu Panel
  val menuPanel = northPanel.menuPanel

  // Canvas Panel
  val scrollPane = panel.scrollPane
  val canvas = scrollPane.canvas

  // Tag List Panel
  val tagListPanel = panel.tagListPanel

  // BUTTONS
  // Tag list button
  val showTagsButton = menuPanel.tagList

  // Navigation buttons
  val inspectButton = navigationPanel.inspectButton
  val clearButton = navigationPanel.clearButton

  // Player buttons
  val player = menuPanel.player
  val playButton = playerButtonsPanel.playButton
  val stopButton = playerButtonsPanel.stopButton
  val startButton = playerButtonsPanel.startButton
  val endButton = playerButtonsPanel.endButton

  // Chart buttons
  val chartsButton = menuPanel.charts
  val lineChartButton = chartsPanel.lineChartButton
  val barChartButton = chartsPanel.barChartButton

  var isRunning = false
  var inSelection = false
  var currentDate = model.life.start
  var x = 0
  var y = 0

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, canvas.keys, slider, showTagsButton, tagListPanel.list.selection, playButton,
    startButton, endButton, stopButton, player, inspectButton, clearButton, chartsButton, lineChartButton, barChartButton)

  view.reactions += {

    case e: MouseClicked =>
      val peer = e.peer
      val clicks = e.clicks
      val point = e.point
      val clicked = new Point(point.getX, point.getY)
      val locations = canvas.locations
      val clickedLocations = locations.filter { location => location.rectangle != None }.flatMap { location =>
        isInRectangle(clicked, location, location.getRectangle())
      }

      if (peer.getButton == java.awt.event.MouseEvent.BUTTON1 && clicks == 2) { // jump
        if (clickedLocations.size > 0) {
          clickedLocations.foreach { location =>
            updateModel(Some(location))
          }
        }

      }
      if (peer.getButton == java.awt.event.MouseEvent.BUTTON3 && clicks == 1) { // select

        if (clickedLocations.size > 0) {
          println("Square in point: " + clickedLocations.size)
          val location = clickedLocations.head
          location.selected = !location.selected
          val tags = location.getTagsAsString()

          updateSelectionInCanvas(location)
        }
      }
    //      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
    //        val point = new Point(e.point.getX, e.point.getY) // + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor
    //        val locations = canvas.model.locations
    //        val ls = locations.filter{location => location.rectangle != null}.flatMap { x => isInRectangle(point, x, x.rectangle)
    //        }
    //
    //        if (ls.size > 0) {
    //          ls.head.ids.foreach { case(id, date) =>
    //            val process: Process = Process("open -a Firefox http://www.stackoverflow.com/questions/" + id).run()
    //            println(process.exitValue())
    //          }
    //        }
    //      }

    case MouseWheelMoved(_, p, _, r) =>
      if (r > 0) {
        canvas.zoomFactor -= 0.1
      } else {
        canvas.zoomFactor += 0.1
      }
      view.repaint()

    case MousePressed(_, p, _, _, _) =>
      x = p.x
      y = p.y

    case MouseDragged(_, p, _) =>
      if (!canvas.changingViewPort) {
        val dx = p.x - x
        val dy = p.y - y

        canvas.offsetX += dx
        canvas.offsetY += dy

        x += dx
        y += dy

        view.repaint()

      } else if (canvas.changingViewPort) {
        val dx = p.x - x
        val dy = p.y - y

        view.repaint()
      }

    case MouseReleased(_, p, _, _, _) =>
      canvas.requestFocus()

    case e: MouseEvent =>
      val point = e.point

      val locations = canvas.locations.filter { location => location.rectangle != None }
      val ls = locations.filter { location =>
        isInRectangle(new Point(point.getX, point.getY), location, location.getRectangle()) match {
          case Some(l) => true
          case None => false
        }
      }

      if (ls.size > 0) {
        val infos = ls.map { location =>
          val tags = location.getTagsAsString()
          val totalCount = location.getTotalCount()
          "Tag: " + tags + "<br>Discussions: " + location.count + "<br>Total Discussions: " + totalCount + "<br>ids: " //+ 
          //          location.ids.map { 
          //            case (date, id) =>
          //              "<br> " + id.map { i => i.toString }.toList.mkString("<br>")
          //              
          //          }
        }.mkString("")
        canvas.tooltip = "<html>" + infos + "</html>"

      } else {
        canvas.tooltip = null
      }

    case KeyReleased(_, Key.BackSpace, _, _) =>
      val head = canvas.locations.head
      val headTags = head.getTagsAsString()
      val index = headTags.lastIndexOf(" ")
      val tree = model.tree

      if (index == -1) {
        updateGradient(tree.root)
        canvas.locations = model.computeModel(Nil, currentDate)

        updateSelectionMenu(canvas.locations)

        val discussionCount = canvas.locations.head.count
        val tagCount = canvas.locations.drop(1).size
        updateMenu(List("Root"), tagCount.toString, discussionCount.toString)
      } else {

        //Retrieve the tags
        val filteredTags = canvas.locations.filter { location => location.selected }.map { location =>
          val tags = location.getTagsAsString()
          val index = tags.lastIndexOf(" ")
          tags.substring(0, index)
        }
        val node = tree.search(tree.root, headTags.substring(0, index).split(" ").toList)
        updateGradient(node)
        canvas.locations = model.computeModel(headTags.substring(0, index).split(" ").toList, currentDate, filteredTags)
        updateSelectionMenu(canvas.locations)
        val tagCount = canvas.locations.drop(1).size
        val discussionCount = canvas.locations.head.count
        updateMenu(headTags.substring(0, index).split(" ").toList, tagCount.toString, discussionCount.toString)
      }

      view.repaint()

    case KeyReleased(_, Key.C, _, _) =>
      clearSelection()

    case KeyReleased(_, Key.M, _, _) =>
      canvas.drawBorders = !canvas.drawBorders
      updateModel()

    case KeyReleased(_, Key.R, _, _) =>
      canvas.offsetX = 0.0
      canvas.offsetY = 0.0
      canvas.zoomFactor = 1.0
      updateModel()

    case ButtonClicked(b) =>
      if (b == inspectButton) {
        inSelection = false
        updateModel()
        inspectButton.enabled = false
      }

      if (b == clearButton) {
        clearSelection()
      }

      if (b == chartsButton) {
        chartsPanel.visible = !chartsPanel.visible
        canvas.requestFocus()
      }

      if (b == player) {
        playerPanel.visible = !playerPanel.visible
        canvas.requestFocus()
      }

      if (b == playButton) {
        isRunning = true
        println("Play")
        val thread = new Thread {
          override def run {
            val life = model.life
            while (isRunning && (currentDate < life.end)) {
              slider.value += 1
              currentDate = incrementDate()
              datePanel.dateLabel.peer.setText(currentDate.toString)
              updateModel()
              canvas.peer.paintImmediately(0, 0, canvas.preferredSize.getWidth.toInt, canvas.preferredSize.getHeight.toInt)
              Thread.sleep(200)
            }
          }
        }.start

      }

      if (b == stopButton) {
        println("Stop")
        isRunning = false
        updateModel()
      }

      if (b == startButton) {
        println("Start")
        val life = model.life
        currentDate = life.start
        updateModel()
        slider.value = slider.min
      }

      if (b == endButton) {
        println("End")
        val life = model.life
        currentDate = life.end
        updateModel()
        slider.value = slider.max
      }

      if (b == lineChartButton) {
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawLineCharGraph(selected, life)
        buildFrame(chart)
      }

      if (b == barChartButton) {
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawBarCharGraph(selected, life)
        buildFrame(chart)
      }

      if (b == showTagsButton) {
        tagListPanel.visible = !tagListPanel.visible
        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(playerPanel.slider) =>
      println("Changed slider")
      val life = slider.life
      currentDate = incrementDate()
      datePanel.dateLabel.peer.setText(currentDate.toString)
      updateModel()

    case SelectionChanged(tagListPanel.list) if (tagListPanel.list.selection.adjusting) =>

      val item = tagListPanel.list.selection.items(0)
      println("Selected from list: " + item)

      val locations = canvas.locations.map { location => location.getTagsAsString() -> location }.toMap
      val location = locations.get(item).get

      println("Selected " + location.getTagsAsString())
      location.selected = !location.selected

      updateSelectionInCanvas(location)
  }

  canvas.focusable = true

  def isInRectangle(point: Point, location: Location, rectangle: ScalaRectangle): Option[Location] = {
    val offset = getOffset()
    val zoom = getZoom()

    val topLeft = (new Point(rectangle.x, rectangle.y) + offset) * zoom
    val bottomRight = (new Point(rectangle.x + rectangle.width, rectangle.y + rectangle.height) + offset) * zoom

    if (point.x < topLeft.x || point.x > bottomRight.x) None
    else if (point.y < topLeft.y || point.y > bottomRight.y) None
    else Some(location)
  }

  def getOffset() = {
    val offsetX = canvas.offsetX
    val offsetY = canvas.offsetY
    new Point(offsetX, offsetY)
  }

  def getZoom() = {
    canvas.zoomFactor
  }

  def updateModel(root: Option[Location] = None) = {
    val locations = canvas.locations

    val head = root match {
      case None => locations.head
      case Some(location) => location
    }

    val tags = head.getTagsAsList()
    val filteredTags = locations.filter { location => location.selected }.map { location => location.getTagsAsString() }

    val tree = model.tree
    val node = tree.search(tree.root, head.getTagsAsList())
    updateGradient(node, filteredTags)

    canvas.locations = model.computeModel(tags, currentDate, filteredTags)
    updateSelectionMenu(canvas.locations)

    val tagCount = canvas.locations.drop(1).size
    val discussionCount = canvas.locations.head.count
    if (tags == Nil) updateMenu(List("Root"), tagCount.toString, discussionCount.toString) else updateMenu(tags, tagCount.toString, discussionCount.toString)
    canvas.requestFocus()
    canvas.repaint()
  }

  def existSelected() = {
    canvas.locations.filter { location => location.selected }.size > 0
  }

  def updateMenu(tags: List[String], tagsCount: String, discussionCount: String) = {

    val pathInfoPanel = mainInfoPanel.pathInfoPanel
    val path = pathInfoPanel.path

    val tagsPanel = currentInfoPanel.currentTagsPanel
    val currentTags = tagsPanel.currentTag

    val discussionsPanel = currentInfoPanel.currentDiscussionsPanel
    val currentDiscussions = discussionsPanel.currentDiscussions

    val text = tags.mkString(" ")
    path.peer.setText(text)
    currentTags.peer.setText(tagsCount)
    currentDiscussions.peer.setText(discussionCount)
  }

  def updateSelectionMenu(locations: List[Location]) = {
    tagListPanel.list.listData = locations.drop(1).map { location => location.getTagsAsString() }.sorted
  }

  def updateGradient(node: Node, tags: List[String] = Nil) = {
    if (tags.size > 0) {
      val tree = model.tree
      val nodes = tags.map { tag => tree.search(tree.root, tag.split(" ").toList) }
      model.fixedRectangles = model.createFixedRectangles(nodes)
      model.maxHeight = model.getMaxCount(nodes)
    } else {
      model.fixedRectangles = model.createFixedRectangles(node.children)
      model.maxHeight = model.getMaxCount(node.children)
    }
  }

  def incrementDate() = {
    val life = model.life
    life.increment(slider.value * life.interval)
  }

  def buildFrame(chart: JFreeChart) = {
    val frame = new Frame {
      preferredSize = new Dimension(1400, 900)
      val panel = new ChartPanel(chart)
      peer.setContentPane(panel)
      peer.setLocationRelativeTo(null)
      peer.setDefaultCloseOperation(DISPOSE_ON_CLOSE)
      peer.pack()
      peer.setVisible(true)
      canvas.requestFocus()
    }
  }

  def clearSelection() = {
    canvas.locations = canvas.locations.map { location =>
      location.selected = false
      location
    }
    inspectButton.enabled = false
    updateModel()
  }

  def updateSelectionInCanvas(location: Location) = {
    val g = canvas.peer.getGraphics
    val rectangle = location.getRectangle()
    if (location.selected) {
      val offset = new Point(canvas.offsetX, canvas.offsetY)
      val point = (new Point(rectangle.x, rectangle.y) + offset) * canvas.zoomFactor

      g.setColor(Color.RED)
      g.fillOval(point.x.toInt, point.y.toInt, 8, 8)
      val width = (rectangle.width - 1) * canvas.zoomFactor
      val height = (rectangle.height - 1) * canvas.zoomFactor
      g.drawRect(point.x.toInt, point.y.toInt, width.toInt, height.toInt)
    } else {
      canvas.requestFocus()
      canvas.repaint()
    }

    if (!existSelected()) {
      inSelection = false
      inspectButton.enabled = false
      updateModel()
    } else {
      canvas.requestFocus()
      inSelection = true
      inspectButton.enabled = true
    }
  }
}
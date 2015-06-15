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
import java.awt.Graphics2D
import org.jfree.chart.ChartPanel
import scala.swing.Frame
import java.awt.Dimension
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import org.jfree.chart.JFreeChart
import java.awt.geom.Rectangle2D
import database.TagFactory
import database.TagTree
import com.github.tototoshi.csv.CSVReader
import java.io.File
import scala.swing.Label
import database.DatabaseRequest
import database.MTree

class Control(val model: Model, val view: View) {
  view.peer.setVisible(true)

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
  val totalInfoPanel = homePanel.totalInfoPanel
  val currentInfoPanel = homePanel.currentInfoPanel
  val navigationPanel = homePanel.navigationPanel
  val chartsPanel = homePanel.chartsPanel
  val loadingPanel = homePanel.loadingPanel

  val intervalPanel = loadingPanel.intervalPanel
  val intervalValue = intervalPanel.intervalValue

  //  val progressPanel = loadingPanel.progressPanel
  //  val progress = progressPanel.progress

  // Menu Panel
  val menuPanel = northPanel.menuPanel

  // Canvas Panel
  val scrollPane = panel.scrollPane
  val canvas = scrollPane.canvas

  // Discussion List Panel
  val discussionPanel = panel.discussionsPanel

  // Tag List Panel
  val tagListPanel = panel.tagListPanel

  // BUTTONS

  // Discussions list button
  val discussionsListButton = menuPanel.discussionsList

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
  val mountainChartButton = chartsPanel.mountainButton

  var isRunning = false
  var inSelection = false
  var currentDate = model.life.start
  var x = 0
  var y = 0

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, canvas.keys, slider, showTagsButton, tagListPanel.list.selection, playButton,
    startButton, endButton, stopButton, player, inspectButton, clearButton, chartsButton, lineChartButton, barChartButton, mountainChartButton, intervalValue.keys, 
    discussionsListButton, discussionPanel.list.selection)

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

          updateSelectionInCanvas()
        }
      }

    case MouseWheelMoved(_, p, _, r) =>
      if (r > 0) {
        canvas.zoomFactor -= 0.1
      } else {
        canvas.zoomFactor += 0.1
      }
      canvas.shapes = canvas.computeShapes()
      canvas.requestFocus()
      canvas.repaint()

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

        canvas.shapes = canvas.computeShapes()
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
          val totalCount = location.total
          val currentCount = location.currentCount
          "Tag: " + tags + "<br>Discussions: " + currentCount + "<br>Total Discussions: " + totalCount + "<br>Discussions in level: "
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
        updateGradient(tree)
        canvas.locations = model.computeModel(tree.value.tags, currentDate)
        canvas.shapes = canvas.computeShapes()

        updateSelectionMenu(canvas.locations)
        updateDiscussionsList(canvas.locations)

        val discussionCount = canvas.locations.head.total
        val tagCount = canvas.locations.drop(1).size
        updateMenu(List("Root"), tagCount.toString, discussionCount.toString)
      } else {

        //Retrieve the tags
        val filteredTags = canvas.locations.filter { location => location.selected }.map { location =>
          val tags = location.getTagsAsString()
          val index = tags.lastIndexOf(" ")
          tags.substring(0, index)
        }
        val target = headTags.substring(0, index).split(" ").toList
        val node = tree.search(target) // ATTENTION
        updateGradient(node)
        canvas.locations = model.computeModel(node.value.tags, currentDate, filteredTags) // ATTENTION
        canvas.shapes = canvas.computeShapes()
        
        updateSelectionMenu(canvas.locations)
        updateDiscussionsList(canvas.locations)
        val tagCount = canvas.locations.drop(1).size
        val discussionCount = canvas.locations.head.total
        updateMenu(target, tagCount.toString, discussionCount.toString)
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

    case KeyPressed(_, Key.Enter, _, _) =>
      intervalValue.editable = false

      val life = model.life
      val value = intervalValue.text
      life.interval = value.toInt
      val date2step = life.getDateMapping()
      
      val tree = model.tree.search(canvas.locations.head.tags)
//      tree.changeCounts(life, date2step)

      model.root = model.tree.value
      val root = model.tree.value

      model.maxHeight = model.getMaxCount(model.tree.children)
      println("(Model) max height: " + model.maxHeight)
      model.fixedRectangles = model.createFixedRectangles(model.tree.children)

      intervalValue.editable = true
      updatePlayer()
      updateModel()

    case ButtonClicked(b) =>
      if (b == inspectButton) {
        updateModel()
        inSelection = false
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
              currentDate = incrementDate()
              datePanel.dateLabel.peer.setText(currentDate.toString)
              slider.value += 1
              updateModel()
//              canvas.peer.paintImmediately(0, 0, canvas.preferredSize.getWidth.toInt, canvas.preferredSize.getHeight.toInt)
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
        slider.value = slider.min
        updateModel()
      }

      if (b == endButton) {
        println("End")
        val life = model.life
        currentDate = life.end
        slider.value = slider.max
        updateModel()
      }

      if (b == lineChartButton) {
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawLineChartGraph(selected, life)
        buildFrame(chart)
      }

      if (b == barChartButton) {
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawBarChartGraph(selected, life)
        buildFrame(chart)
      }
      
      if (b == mountainChartButton) {
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawAreaChartGraph(selected, life)
        buildFrame(chart)
      }

      if (b == showTagsButton) {
        tagListPanel.visible = !tagListPanel.visible
        canvas.requestFocus()
        view.repaint()
      }

      if (b == discussionsListButton) {
        discussionPanel.visible = !discussionPanel.visible
        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(playerPanel.slider) if (!playerPanel.slider.adjusting) =>
        println("Changed slider")
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

      updateSelectionInCanvas()

    case SelectionChanged(discussionPanel.list) if (discussionPanel.list.selection.adjusting) =>

      val item = discussionPanel.list.selection.items(0)
      println("Selected from list: " + item)

      val process: Process = Process("open -a Firefox http://www.stackoverflow.com/questions/" + item).run()
      println(process.exitValue())

      canvas.requestFocus()
  }

  canvas.focusable = true

  //  def buffering(iterator: Iterator[Seq[Seq[String]]], isFirst: Boolean = true) = {
  //    val thread = new Thread {
  //      override def run {
  //        val life = model.life
  //        val date2step = life.getStepsMapping()
  //        while (iterator.hasNext) {
  //          updateProgress()
  //          if (isFirst) {
  //            val vector = TagFactory.mainTagVector(life, iterator.next())
  //            val newVector = model.mainVector ::: vector
  //            model.mainVector = newVector.sortBy { tag => tag.dates2counts.values.max }.reverse
  //
  //            model.tree = TagTree.createTree(model.mainVector)
  //          } else {
  //            iterator.next
  //            model.tree.changeCounts(model.root, life, date2step)
  //          }
  //
  //          val root = model.tree.root
  //          model.maxHeight = model.getMaxCount(model.tree.root.children)
  //          model.fixedRectangles = model.createFixedRectangles(model.tree.root.children)
  //          model.locations = model.computeModel(Nil, life.start)
  //          updateModel()
  //        }
  //
  //        updateProgress()
  //      }
  //    }.start
  //  }

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

    val tags = head.tags
    val filteredTags = locations.filter { location => location.selected }.map { location => location.getTagsAsString() }

    val tree = model.tree
    val node = tree.search(head.tags)
    updateGradient(node, filteredTags)

    canvas.locations = model.computeModel(head.tags, currentDate, filteredTags)
    canvas.shapes = canvas.computeShapes()
    updateSelectionMenu(canvas.locations)
    updateDiscussionsList(canvas.locations)

    val tagCount = canvas.locations.drop(1).size
    val discussionCount = canvas.locations.head.total
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

    val totalTagsPanel = totalInfoPanel.totalTagsPanel
    val totalTags = totalTagsPanel.totalTag

    val discussionsPanel = currentInfoPanel.currentDiscussionsPanel
    val currentDiscussions = discussionsPanel.currentDiscussions

    val text = tags.mkString(" ")
    val newTotalTags = model.getTotalDataset()
    path.peer.setText(text)
    totalTags.peer.setText(newTotalTags.toString)
    currentTags.peer.setText(tagsCount)
    currentDiscussions.peer.setText(discussionCount)
  }

  def updateSelectionMenu(locations: List[Location]) = {
    tagListPanel.list.listData = locations.drop(1).map { location => location.getTagsAsString() }.sorted
  }

  def updateDiscussionsList(locations: List[Location]) = {
    val elements = locations.head.dates2ids.getOrElse(currentDate, (0, Stream()))._2.filter { id => id > 0 }.toList
    discussionPanel.list.listData = elements.map { id => id.toString }
  }

  def updateGradient(tree: MTree, tags: List[String] = Nil) = {
    if (tags.size > 0) {
      val tree = model.tree
      val nodes = tags.map { tag => tree.search(tag.split(" ").toList) }
      model.fixedRectangles = model.createFixedRectangles(nodes)
      model.maxHeight = model.getMaxCount(nodes)
    } else {
      model.fixedRectangles = model.createFixedRectangles(tree.children)
      model.maxHeight = model.getMaxCount(tree.children)
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
    
    if(!inSelection) updateModel()
    else updateSelectionInCanvas()
  }

  def updateSelectionInCanvas() = {
    canvas.shapes = canvas.computeShapes()
    canvas.requestFocus()
    canvas.repaint()

    if (!existSelected()) {
      inSelection = false
      inspectButton.enabled = false
    } else {
      canvas.requestFocus()
      inSelection = true
      inspectButton.enabled = true
    }
  }

  def updatePlayer() = {
    val life = model.life
    val steps = life.steps
    slider.max = steps.size - 1
    slider.value = slider.min

    val checkpoints = (steps.filter { step => step % 50 == 0 } :+ slider.max).distinct
    slider.labels = checkpoints.map { step => step -> new Label(step.toString) }.toMap
  }
}
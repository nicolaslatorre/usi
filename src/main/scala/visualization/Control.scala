package visualization

import java.awt.Dimension

import scala.Stream
import scala.swing.Frame
import scala.swing.Label
import scala.swing.event.ButtonClicked
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.KeyReleased
import scala.swing.event.MouseClicked
import scala.swing.event.MouseDragged
import scala.swing.event.MouseEvent
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased
import scala.swing.event.MouseWheelMoved
import scala.swing.event.SelectionChanged
import scala.swing.event.TableRowsSelected
import scala.swing.event.ValueChanged
import scala.sys.process.Process

import org.jfree.chart.ChartPanel
import org.jfree.chart.JFreeChart

import com.github.nscala_time.time.Imports.richAbstractPartial

import database.DataManagement
import database.MTree
import javax.swing.WindowConstants.DISPOSE_ON_CLOSE
import javax.swing.table.DefaultTableModel

class Control(val model: Model, val view: View) {
  view.peer.setVisible(true)

  //PANELS
  val panel = view.mainPanel

  // South Panel
  val southPanel = panel.southPanel

  // Player Panel
  val playerPanel = southPanel.playerPanel
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

  // Menu Panel
  val menuPanel = northPanel.menuPanel

  // Canvas Panel
  val scrollPane = panel.scrollPane
  val canvas = scrollPane.canvas

  // Discussion List Panel
  val discussionPanel = southPanel.discussionsPanel
  val discussionsTable = discussionPanel.scrollPane.table

  // Tag List Panel
  val tagListPanel = panel.tagListPanel
  val searchPanel = tagListPanel.searchPanel
  val searchField = searchPanel.searchField

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

  var tagsSet: Set[List[String]] = Set()

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, canvas.keys, slider, showTagsButton, tagListPanel.list.selection, playButton,
    startButton, endButton, stopButton, player, inspectButton, clearButton, chartsButton, lineChartButton, barChartButton, mountainChartButton, intervalValue.keys,
    discussionsListButton, discussionsTable.selection, searchField)

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
          canvas.locations.foreach { location =>
            location.selected = false
          }
          
          canvas.drawRelations = false

          clickedLocations.foreach { location =>
            updateModel(Some(location))
          }
        }
      }
      if (peer.getButton == java.awt.event.MouseEvent.BUTTON3 && clicks == 1) { // select

        if (clickedLocations.size > 0) {
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
      canvas.shapes = canvas.computeShapes(currentDate)
      if(canvas.drawRelations) showRelations()
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

        canvas.shapes = canvas.computeShapes(currentDate)
        if(canvas.drawRelations) showRelations()
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
          val currentCount = location.getCurrentCount(currentDate)
          val discussionInLevel = location.getDiscussionInLevel(currentDate)
          "Tag: " + tags + "<br>Discussions: " + currentCount + "<br>Total Discussions: " + totalCount + "<br>Discussions in level: " + discussionInLevel
        }.mkString("")
        canvas.tooltip = "<html>" + infos + "</html>"

      } else {
        canvas.tooltip = null
      }

    case KeyReleased(_, Key.B, _, _) =>
      val head = canvas.locations.head
      val headTags = head.getTagsAsString()
      val index = headTags.lastIndexOf(" ")
      val tree = model.tree
      canvas.drawRelations = false

      if (index == -1) {
        canvas.locations = model.computeModel(tree.value.tags, currentDate)
        updateMaxHeight(tree)
        canvas.shapes = canvas.computeShapes(currentDate)

        updateSelectionMenu(canvas.locations)
        if (discussionPanel.visible) updateDiscussionsList(canvas.locations)

        val discussionCount = canvas.locations.map { location => location.getCurrentCount(currentDate) }.sum
        val tagCount = canvas.locations.drop(1).size
        updateMenu(List("Root"), tagCount.toString, discussionCount.toString)
      } else {
        
        //Retrieve the tags
        
        val target = headTags.substring(0, index).split(" ").toList
        val node = tree.search(target) // ATTENTION

        canvas.locations = model.computeModel(node.value.tags, currentDate) // ATTENTION
        updateMaxHeight(node)
        canvas.shapes = canvas.computeShapes(currentDate)

        updateSelectionMenu(canvas.locations)
        if (discussionPanel.visible) updateDiscussionsList(canvas.locations)
        val tagCount = canvas.locations.drop(1).size
        val discussionCount = canvas.locations.map { location => location.getCurrentCount(currentDate) }.sum
        updateMenu(target, tagCount.toString, discussionCount.toString)
      }

      view.repaint()

    case KeyReleased(_, Key.C, _, _) =>
      clearSelection()

    case KeyReleased(_, Key.A, _, _) =>
      canvas.drawRelations = !canvas.drawRelations
      if (canvas.drawRelations) {
        showRelations()
      } else {
        canvas.relatives = Nil
        canvas.repaint()
        canvas.requestFocus()
      }

    case KeyReleased(_, Key.M, _, _) =>
      canvas.drawBorders = !canvas.drawBorders
      canvas.repaint()
      canvas.requestFocus()

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
      model.date2step = life.getDateMapping()

      val tree = model.tree.search(canvas.locations.head.tags)
      //      tree.changeCounts(life, date2step)

      model.root = model.tree.value
      val root = model.tree.value

      model.maxHeight = model.getMaxCount(model.tree.children)

      intervalValue.editable = true
      inSelection = false
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
        val thread = new Thread {
          override def run {
            val life = model.life
            while (isRunning && (currentDate < life.end)) {
              currentDate = incrementDate()
              datePanel.dateLabel.peer.setText(currentDate.toString)
              slider.value += 1
              updateLocations()
              //              canvas.peer.paintImmediately(0, 0, canvas.preferredSize.getWidth.toInt, canvas.preferredSize.getHeight.toInt)
              Thread.sleep(100)
            }
          }
        }.start

      }

      if (b == stopButton) {
        isRunning = false
        updateLocations()
      }

      if (b == startButton) {
        val life = model.life
        currentDate = life.start
        slider.value = slider.min
        updateLocations()
      }

      if (b == endButton) {
        val life = model.life
        currentDate = life.end
        slider.value = slider.max
        updateLocations()
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
        if (discussionPanel.visible) {
          val locations = canvas.locations
          updateDiscussionsList(locations)
        }
        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(playerPanel.slider) =>
      if (!playerPanel.slider.adjusting) {
        currentDate = incrementDate()
        datePanel.dateLabel.peer.setText(currentDate.toString)
        updateLocations()
      } else {
        currentDate = incrementDate()
        datePanel.dateLabel.peer.setText(currentDate.toString)
      }

    case SelectionChanged(tagListPanel.list) if (tagListPanel.list.selection.adjusting) =>
      val item = tagListPanel.list.selection.items(0)

      val locations = canvas.locations.tail.map { location =>
        val tags = location.tags
        if (tags.size == 1) tags.head -> location
        else tags.last -> location
      }.toMap

      val location = locations.get(item).get

      location.selected = !location.selected

      updateSelectionInCanvas()

    case TableRowsSelected(discussionsTable, range, false) =>
      val index = discussionsTable.selection.rows.mkString("").toInt
      val id = discussionsTable.model.getValueAt(index, 0)

      val process: Process = Process("open http://www.stackoverflow.com/questions/" + id).run()

      canvas.requestFocus()

    case ValueChanged(searchPanel.searchField) =>
      val key = searchField.text
      val locations = canvas.locations
      if (key == "") updateSelectionMenu(locations)
      else {
        val tags = locations.tail.par.map { location => location.tags.last }.toList
        val keyLength = key.size
        val filtered = tags.par.filter { tag => tag.take(keyLength) == key }.toList.sorted
        tagListPanel.list.listData = filtered

      }
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
    val tree = model.tree

    val head = root match {
      case None => locations.head
      case Some(location) =>
        //        if (!tagsSet.contains(location.tags)) {
        //          val children = TagFactory.updateVectorFromTags(model.life, location.tags, model.tree)
        //          tagsSet = tagsSet + location.tags
        //          val node = tree.search(location.tags)
        //          node.children = children
        //        }

        location
    }

    val tags = head.tags
    val filteredTags = locations.filter { location => location.selected }.map { location => location.getTagsAsString() }
    
    
    val node = model.tree.search(head.tags)

    updateMaxHeight(node, filteredTags)

    canvas.locations = model.computeModel(head.tags, currentDate, filteredTags)
    canvas.shapes = canvas.computeShapes(currentDate)
    updateSelectionMenu(canvas.locations)
    if (discussionPanel.visible) updateDiscussionsList(canvas.locations)

    val tagCount = canvas.locations.drop(1).size
    val discussionCount = canvas.locations.map { location => location.getCurrentCount(currentDate) }.sum
    if (tags == Nil) updateMenu(List("Root"), tagCount.toString, discussionCount.toString) else updateMenu(tags, tagCount.toString, discussionCount.toString)
    canvas.requestFocus()
    canvas.repaint()
  }

  def updateLocations(root: Option[Location] = None) = {
    val locations = canvas.locations

    val head = root match {
      case None => locations.head
      case Some(location) => location
    }

    val tags = head.tags
    val filteredTags = locations.filter { location => location.selected }.map { location => location.getTagsAsString() }

    val tree = model.tree
    val node = tree.search(head.tags)

    canvas.locations = model.updateModel(locations, currentDate)
    canvas.shapes = canvas.computeShapes(currentDate)
    updateSelectionMenu(canvas.locations)
    if (discussionPanel.visible) updateDiscussionsList(canvas.locations)

    val tagCount = canvas.locations.drop(1).size
    val discussionCount = canvas.locations.map { location => location.getCurrentCount(currentDate) }.sum
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
    tagListPanel.list.listData = locations.drop(1).map { location =>
      val element = location.tags.last
      element
    }.sorted
  }

  def updateDiscussionsList(locations: List[Location]) = {
    val elements = locations.head.dates2ids.getOrElse(currentDate, (0, Stream()))._2.filter { id => id > 0 }.toSet
    val tags = locations.head.tags

    if (elements.size > 0) {
      val discussions = DataManagement.openDiscussionsFiles(tags, elements).sortBy { discussion => discussion.score }.reverse

      val model = discussions.par.map { discussion =>
        val infos = discussion.getInfo()
        infos.toArray.asInstanceOf[Array[AnyRef]]
      }.toArray

      val headers = Array("ID", "Title", "Creation Date", "Answers", "Score", "View", "Owner").asInstanceOf[Array[AnyRef]]

      discussionsTable.model = new DefaultTableModel(model, headers)
    } else {
      discussionsTable.model = new DefaultTableModel(Array(Array().asInstanceOf[Array[AnyRef]]), Array().asInstanceOf[Array[AnyRef]])

    }
  }

  def updateMaxHeight(tree: MTree, tags: List[String] = Nil) = {
    if (tags.size > 0) {
      val tree = model.tree
      val nodes = tags.map { tag => tree.search(tag.split(" ").toList) }
      model.maxHeight = model.getMaxCount(nodes)
    } else {
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

    if (!inSelection) updateModel()
    else updateSelectionInCanvas()
  }

  def updateSelectionInCanvas() = {
    canvas.shapes = canvas.computeShapes(currentDate)
    

    if (!existSelected()) {
      inSelection = false
      inspectButton.enabled = false
      canvas.drawRelations = false
    } else {
      canvas.requestFocus()
      inSelection = true
      inspectButton.enabled = true
    }
    canvas.requestFocus()
    canvas.repaint()
  }

  def updatePlayer() = {
    val life = model.life
    val steps = life.steps
    slider.max = steps.size - 1
    slider.value = slider.min

    val checkpoints = (steps.filter { step => step % 50 == 0 } :+ slider.max).distinct
    slider.labels = checkpoints.map { step => step -> new Label(step.toString) }.toMap
  }

  def showRelations() = {
    val locations = canvas.locations
    val selected = locations.filter { location => location.selected }

    val related = selected.map { s => s -> relations(s) }.toMap
    canvas.relatives = canvas.computeRelated(related)
    canvas.requestFocus()
    canvas.repaint()
  }

  def relations(location: Location) = {
    val tree = model.tree
    val locations = canvas.locations
    val startLevel = tree.search(locations.head.tags)
    val targetTags = location.tags
    val childrens = startLevel.children.filter { child => child.value.tags != targetTags } // filter out the selected location
    val related = childrens.par.flatMap { child =>
      val contained = child.hasRelation(targetTags.last)
      if (contained) Option(child)
      else None
    }.toList

    val ts = related.flatMap { r =>
      List(r.value.tags)
    }

    val rel = locations.filter { location =>
      val tags = location.tags

      ts.contains(tags)
    }

    rel
  }
}
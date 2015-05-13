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
  val canvas = view.panel.canvas
  val buttons = view.panel.sliderPanel.buttonPanel
  val slider = view.panel.sliderPanel.slider
  val selectionMenu = view.panel.selectionMenu
  val showButton = view.panel.menuEast.showList

  var isRunning = false
  var inSelection = false

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, view.panel.canvas.keys, showButton, selectionMenu.list.selection, buttons.playButton,
    buttons.startButton, buttons.endButton, buttons.stopButton, buttons.stopButton, buttons.selectionButton, slider, buttons.monthInterval.monthValue.keys, buttons.graphButton,
    buttons.graphLineButton)
  var x = 0
  var y = 0

  view.reactions += {

    case e: MouseClicked =>
      val nrClicks = e.clicks
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && nrClicks == 2) { // jump

        val point = new Point(e.point.getX, e.point.getY)
        val locations = canvas.locations

        val ls = locations.filter { x => x.rectangle != null }.flatMap { x =>
          isInRectangle(point, x, x.rectangle)
        }

        if (ls.size > 0) {
          println("Square in point: " + ls.size)
          ls.foreach { x =>
            println("Jumping into " + x.tags)
            val tree = model.tree
            val node = tree.search(tree.root, x.getTagsAsList())
            updateGradient(node)
            canvas.locations = model.computeModel(x.tags, model.currentDate)
            updateSelectionMenu()
            updateMenu(x.tags, x.count.toString)

            view.repaint()
          }
        }

      }
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1 && nrClicks == 1) { // select
        val point = new Point(e.point.getX, e.point.getY)
        val locations = canvas.locations

        val ls = locations.filter { x => x.rectangle != null }.flatMap { x =>
          isInRectangle(point, x, x.rectangle)
        }

        if (ls.size > 0) {
          println("Square in point: " + ls.size)
          ls.foreach { x =>
            println("Selected " + x.tags)
            x.selected = !x.selected

            val g = canvas.peer.getGraphics
            val rect = x.rectangle
            if (x.selected) {
              val offset = getOffset()
              val zoom = getZoom()

              val pointOval = (new Point(rect.x, rect.y) + offset) * zoom

              g.setColor(Color.RED)
              g.fillOval(pointOval.x.toInt, pointOval.y.toInt, 8, 8)
              val width = (x.rectangle.width-1) * canvas.zoomFactor
              val height = (x.rectangle.height-1) * canvas.zoomFactor
              g.drawRect(pointOval.x.toInt, pointOval.y.toInt, width.toInt, height.toInt)
            } else {
              canvas.repaint()
            }
          }

          if (!existSelected()) {
            inSelection = false
            buttons.selectionButton.visible = false
            updateModel()
            canvas.repaint()
          } else {
            inSelection = true
            buttons.selectionButton.visible = true
          }
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

      println("CURRENT ZOOM: " + canvas.zoomFactor)

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

      val locations = canvas.locations.filter { x => x.rectangle != null }
      val ls = locations.filter { x =>
        isInRectangle(new Point(point.getX, point.getY), x, x.rectangle) match {
          case Some(l) => true
          case None => false
        }
      }

      if (ls.size > 0) {
        val infos = ls.map { location =>
          "tag: " + location.tags + "<br>occurrences: " + location.count + "<br>total occurrences: " + location.totalCount + "<br>ids: " //+ 
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
      val index = head.tags.lastIndexOf(" ")

      val tree = model.tree

      if (index == -1) {
        updateGradient(tree.root)
        canvas.locations = model.computeModel("", model.currentDate)

        updateSelectionMenu()
        updateMenu("Stack Overflow", canvas.locations.head.count.toString)
      } else {

        //Retrieve the tags
        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc =>
          val tags = loc.tags
          val index = tags.lastIndexOf(" ")
          tags.substring(0, index)
        }
        val node = tree.search(tree.root, head.tags.substring(0, index).split(" ").toList)
        updateGradient(node)
        canvas.locations = model.computeModel(head.tags.substring(0, index), model.currentDate, filteredTags)
        updateSelectionMenu()
        updateMenu(canvas.locations.head.tags, canvas.locations.head.count.toString)
      }

      view.repaint()

    case KeyReleased(_, Key.C, _, _) =>
      canvas.locations = canvas.locations.map { location =>
        location.selected = false
        location
      }

      updateModel()
      buttons.selectionButton.visible = false
      view.repaint()

    case KeyReleased(_, Key.M, _, _) =>
      canvas.drawBorders = !canvas.drawBorders

      updateModel()
      view.repaint()

    case KeyReleased(_, Key.R, _, _) =>
      canvas.offsetX = 0.0
      canvas.offsetY = 0.0
      canvas.zoomFactor = 1.0

      updateModel()
      view.repaint()

    case ButtonClicked(b) =>
      if (b == buttons.selectionButton) {
        println("Selection")
        inSelection = false
        updateModel()

        buttons.selectionButton.visible = false

        canvas.requestFocus()
        view.repaint()

      }
      if (b == view.panel.sliderPanel.buttonPanel.playButton) {
        isRunning = true
        println("Play")
        val thread = new Thread {
          override def run {
            val life = model.life
            while (isRunning && (model.currentDate < life.end)) {
              slider.value += 1

              val head = canvas.locations.head
              model.currentDate = incrementDate()

              val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }
              println("(Control) tags: " + filteredTags)

              canvas.locations = model.computeModel(head.tags, model.currentDate, filteredTags)
              updateSelectionMenu()

              buttons.dateLabel.peer.setText(model.currentDate.toString)
              updateMenu(head.tags, head.count.toString)
              canvas.requestFocus()
              //canvas.peer.paintImmediately(0, 0, 1440, 900)
              canvas.peer.paintImmediately(0, 0, canvas.preferredSize.getWidth.toInt, canvas.preferredSize.getHeight.toInt)

              Thread.sleep(200)
            }

          }
        }.start

      }

      if (b == view.panel.sliderPanel.buttonPanel.stopButton) {
        println("Stop")
        isRunning = false

        val head = canvas.locations.head
        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)
        //        val valueDate = model.startDate
        //        slider.value = model.months.getOrElse(valueDate, 0)

        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.startButton) {
        println("Start")
        val life = model.life
        val head = canvas.locations.head
        model.currentDate = life.start

        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }

        canvas.locations = model.computeModel(head.tags, model.currentDate, filteredTags)
        updateSelectionMenu()

        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)

        slider.value = slider.min
        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.endButton) {
        println("End")
        val head = canvas.locations.head
        val life = model.life
        model.currentDate = life.end

        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }

        canvas.locations = model.computeModel(head.tags, model.currentDate, filteredTags)
        updateSelectionMenu()

        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)

        slider.value = slider.max
        canvas.requestFocus()
        view.repaint()
      }

      if (b == buttons.graphLineButton) {
        println("Line")
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawLineCharGraph(selected, life)
        buildFrame(chart)

        canvas.requestFocus()
      }

      if (b == buttons.graphButton) {
        println("Bar")
        val head = canvas.locations.head
        val life = model.life
        val selected = canvas.locations.filter { location => location.selected }

        val chart = Graph.drawBarCharGraph(selected, life)
        buildFrame(chart)

        canvas.requestFocus()
      }

      if (b == showButton) {
        println("Showing list")
        selectionMenu.visible = !selectionMenu.visible

        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(view.panel.sliderPanel.slider) =>
      println("Changed slider")
      val life = slider.life
      model.currentDate = incrementDate()

      buttons.dateLabel.peer.setText(model.currentDate.toString)

      val head = canvas.locations.head
      val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }
      canvas.locations = model.computeModel(head.tags, model.currentDate, filteredTags)
      updateSelectionMenu()
      if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)

      canvas.requestFocus()
      view.repaint()

    case SelectionChanged(selectionMenu.list) if (selectionMenu.list.selection.adjusting) =>

      val item = selectionMenu.list.selection.items(0)
      println("Selected from list: " + item)

      val locations = canvas.locations.map { location => location.tags -> location }.toMap
      val location = locations.get(item).get

      println("Selected " + location.tags)
      location.selected = !location.selected

      val g = canvas.peer.getGraphics
      val rect = location.rectangle
      if (location.selected) {
        val offset = new Point(canvas.offsetX, canvas.offsetY)
        val point = (new Point(rect.x, rect.y) + offset) * canvas.zoomFactor
        
        g.setColor(Color.RED)
        g.fillOval(point.x.toInt, point.y.toInt, 8, 8)
        val width = (rect.width-1) * canvas.zoomFactor
        val height = (rect.height-1) * canvas.zoomFactor
        g.drawRect(point.x.toInt, point.y.toInt, width.toInt, height.toInt)
      } else {
        canvas.requestFocus()
        canvas.repaint()
      }

      if (!existSelected()) {
        inSelection = false
        buttons.selectionButton.visible = false
        updateModel()
        canvas.requestFocus()
        canvas.repaint()
      } else {
        canvas.requestFocus()
        inSelection = true
        buttons.selectionButton.visible = true
      }
  }

  view.panel.canvas.focusable = true

  def isInRectangle(point: Point, location: Location, rect: Rectangle): Option[Location] = {
    val offset = getOffset()
    val zoom = getZoom()

    val topLeft = (new Point(rect.x, rect.y) + offset) * zoom
    val bottomRight = (new Point(rect.x + rect.width, rect.y + rect.height) + offset) * zoom

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

  def updateModel() = {
    val head = canvas.locations.head
    val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }

    val tree = model.tree
    val node = tree.search(tree.root, head.getTagsAsList())
    updateGradient(node, filteredTags)

    canvas.locations = model.computeModel(head.tags, model.currentDate, filteredTags)
    updateSelectionMenu()
    if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)
    view.panel.menuEast.occurrences.peer.setText(head.count.toString)
  }

  def existSelected() = {
    canvas.locations.filter { location => location.selected }.size > 0
  }

  def updateMenu(text: String, count: String) = {
    view.panel.menuEast.text.peer.setText(text)
    view.panel.menuEast.occurrences.peer.setText(count)
  }

  def updateSelectionMenu() = {
    selectionMenu.list.listData = canvas.locations.drop(1).map { location => location.tags }.sorted
    selectionMenu.number.peer.setText("Number of tags: " + canvas.locations.drop(1).size)
  }

  def updateSlider() = {

  }

  def updateGradient(node: Node, tags: List[String] = Nil) = {
    if (tags.size > 0) {

      val tree = model.tree
      val nodes = tags.map { tag => tree.search(tree.root, tag.split(" ").toList) }
      model.tot = nodes.map { node => node.tag.getMaxCount() }.sum
      model.fixedRectangles = model.createFixedRectangles(nodes, model.tot)

      model.maxHeight = model.getMaxCount(nodes)
      model.currentGradient = model.gradient.createGradient(model.maxHeight)
    } else {
      model.tot = node.children.map { node => node.tag.getMaxCount() }.sum
      model.fixedRectangles = model.createFixedRectangles(node.children, model.tot)

      model.maxHeight = model.getMaxCount(node.children)
      model.currentGradient = model.gradient.createGradient(model.maxHeight)

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
    }
  }
}
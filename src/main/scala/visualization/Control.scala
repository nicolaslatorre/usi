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

class Control(val model: Model, val view: View) {
  val canvas = view.panel.canvas
  val buttons = view.panel.sliderPanel.buttonPanel
  val slider = view.panel.sliderPanel.slider
  val selectionMenu = view.panel.selectionMenu
  val showButton = view.panel.menuEast.showList

  var isRunning = false
  var inSelection = false

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, view.panel.canvas.keys, showButton, selectionMenu.list.selection, buttons.playButton,
    buttons.startButton, buttons.endButton, buttons.stopButton, buttons.stopButton, buttons.selectionButton, slider, buttons.monthInterval.monthValue.keys)
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
            canvas.locations = model.computeModel(x.tags, model.startDate)
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
              g.setColor(Color.RED)
              g.fillOval(rect.x.toInt, rect.y.toInt, 8, 8)
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
      if (index == -1) {
        canvas.locations = model.computeModel("", model.startDate)
        updateSelectionMenu()
        updateMenu("Stack Overflow", canvas.locations.head.count.toString)
      } else {

        //Retrieve the tags
        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc =>
          val tags = loc.tags
          val index = tags.lastIndexOf(" ")
          tags.substring(0, index)
        }
        canvas.locations = model.computeModel(head.tags.substring(0, index), model.startDate, filteredTags)
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
            while (isRunning && (model.startDate < model.endDate)) {
              val head = canvas.locations.head
              model.startDate = model.startDate.plusMonths(model.interval) //.plusDays(1)

              val valueDate = model.startDate
              slider.value = model.months.get(valueDate).get

              val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }
              println("(Control) tags: " + filteredTags)

              canvas.locations = model.computeModel(head.tags, model.startDate, filteredTags)
              updateSelectionMenu()

              buttons.dateLabel.peer.setText(model.startDate.toString)
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
        //canvas.peer.paintImmediately(0, 0, 1440, 900)
        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)
        val valueDate = model.startDate
        slider.value = model.months.getOrElse(valueDate, 0)

        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.startButton) {
        println("Start")
        val head = canvas.locations.head
        model.startDate = new LocalDate(2008, 7, 31).withDayOfMonth(1)

        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }

        canvas.locations = model.computeModel(head.tags, model.startDate, filteredTags)
        updateSelectionMenu()

        //        canvas.locations = model.computeModel(head.tags, model.startDate)

        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)

        //        canvas.locations = canvas.model.computeModel("", canvas.model.startDate)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        val valueDate = model.startDate
        slider.value = model.months.getOrElse(valueDate, 0)
        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.endButton) {
        println("End")
        val head = canvas.locations.head
        model.startDate = model.endDate

        val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }

        canvas.locations = model.computeModel(head.tags, model.startDate, filteredTags)
        updateSelectionMenu()

        //        canvas.locations = model.computeModel(head.tags, model.startDate)

        if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        slider.value = slider.max
        canvas.requestFocus()
        view.repaint()
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
      model.startDate = life.increment(life.incrementByMonth, slider.value * model.interval)
      
      buttons.dateLabel.peer.setText(model.startDate.toString)

      val head = canvas.locations.head
      val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }
      canvas.locations = model.computeModel(head.tags, model.startDate, filteredTags)
      updateSelectionMenu()
      if (head.tags == "") updateMenu("Stack Overflow", head.count.toString) else updateMenu(head.tags, head.count.toString)

      canvas.requestFocus()
      view.repaint()

    case KeyPressed(_, Key.Enter, _, _) =>
      println("Changed month interval")
      val head = canvas.locations.head
      val value = buttons.monthInterval.monthValue.peer.getText
      model.interval = buttons.monthInterval.monthValue.peer.getText.toInt

      canvas.locations = model.computeModel(head.tags, model.startDate)

      buttons.dateLabel.peer.setText(model.startDate.toString)

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
        g.setColor(Color.RED)
        g.fillOval(rect.x.toInt, rect.y.toInt, 8, 8)
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

    
    val topLeft = new Point(rect.x, rect.y)
    val bottomRight = new Point(rect.x + rect.width, rect.y + rect.height)

    if (point.x < topLeft.x || point.x > bottomRight.x) None
    else if (point.y < topLeft.y || point.y > bottomRight.y) None
    else Some(location)
  }

  def updateModel() = {
    val head = canvas.locations.head
    val filteredTags = canvas.locations.filter { location => location.selected }.map { loc => loc.tags }
    canvas.locations = model.computeModel(head.tags, model.startDate, filteredTags)
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
}
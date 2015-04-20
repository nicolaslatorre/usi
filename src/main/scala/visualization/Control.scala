package visualization

import scala.swing.event.MouseClicked
import scala.swing.event.MouseEvent
import com.github.nscala_time.time.Imports._
import scala.swing.event.ButtonClicked
import scala.swing.event.MousePressed
import scala.swing.event.MouseDragged
import scala.swing.event.ValueChanged
import scala.swing.event.KeyReleased
import scala.swing.event.MouseReleased
import scala.swing.event.MouseWheelMoved
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import java.awt.Graphics2D

class Control(val model: Model, val view: View) {
  val canvas = view.panel.canvas
  val buttons = view.panel.sliderPanel.buttonPanel
  val slider = view.panel.sliderPanel.slider
  
  var isRunning = false

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, view.panel.canvas.keys, buttons.playButton,
    buttons.startButton, buttons.endButton, buttons.stopButton, buttons.stopButton, slider, buttons.monthInterval.monthValue.keys)
  var x = 0
  var y = 0

  view.reactions += {
    case e: MouseClicked =>
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
        val point = new Point(e.point.getX, e.point.getY)
        val locations = canvas.locations

        val ls = locations.filter { x => x.rect != null }.flatMap { x =>
          isInRectangle(point, x, x.rect)
        }

        if (ls.size > 0) {
          println("Square in point: " + ls.size)
          ls.foreach { x =>
            println("Jumping into " + x.tags)
            canvas.locations = model.computeModel(x.tags, model.startDate)

            view.panel.menuEast.text.peer.setText(x.tags)
            view.panel.menuEast.occurrences.peer.setText(x.count.toString)

            view.repaint()
          }
        }

      }
    //      else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
    //        val point = new Point(e.point.getX, e.point.getY) // + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor
    //        val locations = canvas.model.locations
    //        val ls = locations.flatMap { x => isInLocation(point, x, 0)
    //        }
    //
    //        if (ls.size > 0) {
    //          ls.foreach { x =>
    //            val process: Process = Process("open -a Firefox http://www.stackoverflow.com/questions/" + x.id).run()
    //            println(process.exitValue())
    //          }
    //        }
    //      }

    case e: MouseEvent =>
      val point = e.point
      //      println("(" + point.getX + ", " + point.getY + ")")

      val locations = canvas.locations.filter { x => x.rect != null }
      val ls = locations.filter { x =>
        isInRectangle(new Point(point.getX, point.getY), x, x.rect) match {
          case Some(l) => true
          case None => false
        }
      }

      if (ls.size > 0) {
        val infos = ls.map { location =>
          "tag: " + location.tags + "<br>occurrences: " + location.count
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
        view.panel.menuEast.text.peer.setText("Stack Overflow")
        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)
      } else {
        canvas.locations = model.computeModel(head.tags.substring(0, index), model.startDate)
        view.panel.menuEast.text.peer.setText(canvas.locations.head.tags)
        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)
      }

      view.repaint()

    case ButtonClicked(b) =>
      if (b == view.panel.sliderPanel.buttonPanel.playButton) {
        isRunning = true
        println("Play")
        val thread = new Thread {
          override def run {
            while (isRunning && (model.startDate < new LocalDate(2015, 3, 8).minusMonths(1))) {
              val head = canvas.locations.head
              model.startDate = model.startDate.plusDays(1)

              val interval = new Interval(slider.start.toDate().getTime, model.startDate.plusMonths(model.interval).toDate().getTime)
              slider.value = interval.toDuration().getStandardDays.toInt

              canvas.locations = model.computeModel(head.tags, model.startDate)

              buttons.dateLabel.peer.setText(model.startDate.toString)
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
        if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)
        view.panel.menuEast.occurrences.peer.setText(head.count.toString)
        val interval = new Interval(slider.start.toDate().getTime, model.startDate.toDate().getTime)
        slider.value = interval.toDuration().getStandardDays.toInt

        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.startButton) {
        println("Start")
        val head = canvas.locations.head
        model.startDate = new LocalDate(2008, 7, 31)

        canvas.locations = model.computeModel(head.tags, model.startDate)

        if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)
        view.panel.menuEast.occurrences.peer.setText(head.count.toString)

        //        canvas.locations = canvas.model.computeModel("", canvas.model.startDate)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        val interval = new Interval(slider.start.toDate().getTime, model.startDate.toDate().getTime)
        slider.value = interval.toDuration().getStandardDays.toInt
        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.endButton) {
        println("End")
        val head = canvas.locations.head
        model.startDate = model.endDate.minusMonths(model.interval)

        canvas.locations = model.computeModel(head.tags, model.startDate)

        if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)

        view.panel.menuEast.occurrences.peer.setText(head.count.toString)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        slider.value = slider.max
        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(view.panel.sliderPanel.slider) =>
      println("Changed slider")
      model.startDate = slider.start.plusDays(slider.value)
      buttons.dateLabel.peer.setText(model.startDate.toString)
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
  }

  view.panel.canvas.focusable = true

  def isInRectangle(point: Point, location: Location, rect: Rectangle): Option[Location] = {

    val xs = (rect.x to (rect.x + rect.width)).toStream
    val ys = (rect.y to (rect.y + rect.height)).toStream

    val coordinates = xs.flatMap { x => Stream.continually(x) zip ys }.toSet

    if (coordinates.contains((point.x.toInt, point.y.toInt))) Some(location)
    else None
  }
}
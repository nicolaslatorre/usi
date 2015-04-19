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

class Control(val model: Model, val view: View) {
  val canvas = view.panel.canvas
  val buttons = view.panel.sliderPanel.buttonPanel
  val slider = view.panel.sliderPanel.slider
  var isRunning = false

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, view.panel.canvas.keys, buttons.playButton,
    buttons.startButton, buttons.endButton, buttons.stopButton, buttons.stopButton, slider)
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
            canvas.locations = canvas.model.computeModel(x.tags, canvas.model.startDate)

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

    case MousePressed(_, p, _, _, _) =>
      x = p.x
      y = p.y
      if (canvas.changingViewPort) {
        canvas.viewPortX = x
        canvas.viewPortY = y
      }
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

        println("possibleZoom: " + (canvas.preferredSize.getWidth / dx))

        canvas.viewPortWidth = dx
        canvas.viewPortHeight = ((canvas.preferredSize.getHeight * dx) / canvas.preferredSize.getWidth).toInt

        view.repaint()
      }

    case MouseReleased(_, p, _, _, _) =>
      if (canvas.changingViewPort) {
        val dx = p.x - x
        val dy = p.y - y

        canvas.offsetX -= (x / canvas.zoomFactor)
        canvas.offsetY -= (y / canvas.zoomFactor)

        val width = canvas.preferredSize.getWidth
        val height = canvas.preferredSize.getHeight

        val d = width / height
        val h = (height * dx) / width

        //val steps = ((width - dx)/100).toInt
        val steps = width / dx
        val steps2 = dy * (height / dy)

        println("new zoom: " + steps)
        canvas.zoomFactor += 0.1 * steps

        resetViewPortSelection()

        view.repaint()

      }

    case MouseWheelMoved(_, p, _, r) =>
      if (r > 0) {
        canvas.zoomFactor -= 0.1
      } else {
        canvas.zoomFactor += 0.1
      }

      println("CURRENT ZOOM: " + canvas.zoomFactor)

      view.repaint()

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

    case KeyPressed(_, Key.R, _, _) =>
      println("Reset")
      canvas.locations = canvas.model.computeModel("", new LocalDate(2008, 8, 31))
      view.panel.menuEast.text.peer.setText("Stack Overflow")
      view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)
      view.repaint()

    case KeyPressed(_, Key.M, _, _) =>
      println("Draw messages")
      canvas.drawMessages = !canvas.drawMessages
      view.repaint()

    case KeyPressed(_, Key.A, _, _) =>
      println("Draw all messages")
      canvas.drawAllMessages = !canvas.drawAllMessages
      view.repaint()

    case KeyPressed(_, Key.C, _, _) =>
      println("Changing viewport")
      canvas.changingViewPort = true
      view.repaint()

    case KeyReleased(_, Key.C, _, _) =>
      println("Stop changing viewport")
      canvas.changingViewPort = false
      view.repaint()

    case KeyReleased(_, Key.BackSpace, _, _) =>
      val head = canvas.locations.head
      val index = head.tags.lastIndexOf(" ")
      if (index == -1) {
        canvas.locations = canvas.model.computeModel("", canvas.model.startDate)
        view.panel.menuEast.text.peer.setText("Stack Overflow")
        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)
      } else {
        canvas.locations = canvas.model.computeModel(head.tags.substring(0, index), canvas.model.startDate)
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
            while (isRunning && (canvas.model.startDate < new LocalDate(2015, 3, 9))) {
              val head = canvas.locations.head
              canvas.model.startDate = canvas.model.startDate.plusDays(1)

              val interval = new Interval(slider.start.toDate().getTime, canvas.model.startDate.toDate().getTime)
              slider.value = interval.toDuration().getStandardDays.toInt

              canvas.locations = canvas.model.computeModel(head.tags, canvas.model.startDate)
              canvas.requestFocus()
              view.repaint()
            }

          }
        }.start

      }

      if (b == view.panel.sliderPanel.buttonPanel.stopButton) {
        println("Stop")
        isRunning = false
        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.startButton) {
        println("Start")
        val head = canvas.locations.head
        canvas.model.startDate = new LocalDate(2008, 8, 31)

        canvas.locations = canvas.model.computeModel(head.tags, canvas.model.startDate)

        if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)
        view.panel.menuEast.occurrences.peer.setText(head.count.toString)

        //        canvas.locations = canvas.model.computeModel("", canvas.model.startDate)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        val interval = new Interval(slider.start.toDate().getTime, canvas.model.startDate.toDate().getTime)
        slider.value = interval.toDuration().getStandardDays.toInt
        canvas.requestFocus()
        view.repaint()
      }

      if (b == view.panel.sliderPanel.buttonPanel.endButton) {
        println("End")
        val head = canvas.locations.head
        canvas.model.startDate = new LocalDate(2015, 3, 9)

        canvas.locations = canvas.model.computeModel(head.tags, canvas.model.startDate)

        if (head.tags == "") view.panel.menuEast.text.peer.setText("Stack Overflow") else view.panel.menuEast.text.peer.setText(head.tags)

        view.panel.menuEast.occurrences.peer.setText(head.count.toString)
        //        view.panel.menuEast.text.peer.setText("Stack Overflow")
        //        view.panel.menuEast.occurrences.peer.setText(canvas.locations.head.count.toString)

        slider.value = slider.max
        canvas.requestFocus()
        view.repaint()
      }

    case ValueChanged(view.panel.sliderPanel.slider) =>
      println("Changed mf")

  }

  view.panel.canvas.focusable = true

  def isInRectangle(point: Point, location: Location, rect: Rectangle): Option[Location] = {

    val xs = (rect.x to (rect.x + rect.width)).toStream
    val ys = (rect.y to (rect.y + rect.height)).toStream

    val coordinates = xs.flatMap { x => Stream.continually(x) zip ys }.toSet

    if (coordinates.contains((point.x.toInt, point.y.toInt))) Some(location)
    else None
  }

  def resetViewPortSelection() = {
    canvas.viewPortX = 0
    canvas.viewPortY = 0
    canvas.viewPortWidth = 0
    canvas.viewPortHeight = 0
  }

}
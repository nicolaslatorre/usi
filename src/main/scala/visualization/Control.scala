package visualization

import java.awt.MouseInfo
import scala.swing.event.MouseClicked
import scala.swing.event.MousePressed
import scala.swing.event.MouseDragged
import scala.swing.event.MouseWheelMoved
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import scala.sys.process._
import scala.swing.event.MouseEvent
import scala.swing.event.KeyReleased
import scala.swing.event.MouseReleased

class Control(val model: Model, val view: View) {
  val canvas = view.panel.canvas

  view.listenTo(canvas, canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, view.panel.canvas.keys)
  var x = 0
  var y = 0


  view.reactions += {
    case e: MouseClicked =>
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
        val point = new Point(e.point.getX, e.point.getY) // + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor
        val locations = canvas.model.locations
        val ls = locations.flatMap { x => isInLocation(point, x, x.ray)
        }

        if (ls.size > 0) {
          ls.foreach { x =>
            val process: Process = Process("open -a Firefox http://www.stackoverflow.com/questions/" + x.id).run()
            println(process.exitValue())
          }
        }
      }

    case MousePressed(_, p, _, _, _) =>
      x = p.x
      y = p.y
      if(canvas.changingViewPort) {
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
        
        println("possibleZoom: " + (canvas.preferredSize.getWidth/dx))
        
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

      val locations = canvas.model.locations
      val ls = locations.filter { x =>
        isInLocation(new Point(point.getX, point.getY), x, x.ray) match {
          case Some(l) => true
          case None => false
        }
      }

      if (ls.size > 0) {
        val infos = ls.map { location =>
          "Id: " + location.id + "<br>Title: " + location.title + "<br>Tags: " + location.tags + "<br>answers: " + location.answerCount + "<br>Creation Date: " + location.date + "<br><br>"
        }.mkString("")
        canvas.tooltip = "<html>" + infos + "</html>"

      } else {
        canvas.tooltip = null
      }

    case KeyPressed(_, Key.R, _, _) =>
      println("Reset")
      canvas.offsetX = 0.0
      canvas.offsetY = 0.0
      canvas.zoomFactor = 0.1
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

  }

  view.panel.canvas.focusable = true

  def isInLocation(point: Point, location: Location, ray: Int): Option[Location] = {
    val distance = point.distance((location.center + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor)
    if (distance <= (ray * canvas.zoomFactor)) Some(location)
    else None
  }
  
  def resetViewPortSelection() = {
    canvas.viewPortX = 0
    canvas.viewPortY = 0
    canvas.viewPortWidth = 0
    canvas.viewPortHeight = 0
  }

}
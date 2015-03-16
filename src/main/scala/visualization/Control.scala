package visualization

import scala.swing.Dialog
import scala.swing.Label
import scala.swing.Panel
import scala.swing.event.MouseClicked
import scala.swing.event.MousePressed
import scala.swing.event.MouseDragged
import scala.swing.event.MouseWheelMoved
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import java.awt.Dimension

class Control(val model: Model, val view: View) {
  //view.peer.setVisible(true)

  val canvas = view.panel.canvas

  view.listenTo(canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, canvas.keys)
  var x = 0
  var y = 0

  view.reactions += {
    case e: MouseClicked =>
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
        val point = new Point(e.point.getX, e.point.getY) // + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor
        val locations = canvas.locations
        val ls = locations.flatMap { x =>
          if (!canvas.drawWithEqualRay) isInLocation(point, x, x.ray)
          else isInLocation(point, x, canvas.defaultRay.toInt)
        }

        if (ls.size > 0) {
          ls.foreach { x => Dialog.showMessage(view.panel, x.tags, x.name) }

          println("Button3")
        }
      }

    case MousePressed(_, p, _, _, _) =>
      x = p.x
      y = p.y
    case MouseDragged(_, p, _) =>
      val dx = p.x - x
      val dy = p.y - y

      canvas.offsetX += dx
      canvas.offsetY += dy

      x += dx
      y += dy

      view.repaint()

    case MouseWheelMoved(_, p, _, r) =>
      if (r > 0) {
        canvas.zoomFactor -= 0.1
      } else {
        canvas.zoomFactor += 0.1       
      }

      view.repaint()

    case KeyPressed(_, Key.R, _, _) =>
      println("Reset")
      canvas.offsetX = 0.0
      canvas.offsetY = 0.0
      canvas.zoomFactor = 1.0
      view.repaint()

    case KeyPressed(_, Key.C, _, _) =>
      println("Change rays")
      canvas.drawWithEqualRay = !canvas.drawWithEqualRay
      println(canvas.drawWithEqualRay)
      view.repaint()
  }

  view.panel.canvas.focusable = true

  def isInLocation(point: Point, location: Location, ray: Int): Option[Location] = {
    val distance = point.distance((location.center + Point(canvas.offsetX, canvas.offsetY)) * canvas.zoomFactor)
    if (distance <= (ray*canvas.zoomFactor)) Some(location)
    else None
  }

}
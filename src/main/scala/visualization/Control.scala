package visualization

import scala.swing.event._

class Control(val model: Model, val view: View) {
  //view.peer.setVisible(true)

  val canvas = view.panel.canvas

  view.listenTo(canvas.mouse.clicks, canvas.mouse.moves, canvas.mouse.wheel, canvas.keys)
  var x = 0
  var y = 0

  view.reactions += {
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
      if (r > 0) canvas.zoomFactor += 0.1
      else canvas.zoomFactor -= 0.1

      view.repaint()

    case KeyPressed(_, Key.R, _, _) =>
      println("pressed R")
      canvas.offsetX = 0.0
      canvas.offsetY = 0.0
      view.repaint()
  }

}
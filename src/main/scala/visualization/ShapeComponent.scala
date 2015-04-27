package visualization

import java.awt.Shape
import scala.swing.Component
import java.awt.Dimension
import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.Rectangle2D

class ShapeComponent(val shape: Shape, val color: Color, val loc: Location, val key: Int) extends Component {

  preferredSize = getPreferredSize()
  foreground = color

  def getPreferredSize() = {
    val insets = peer.getInsets
    val bounds = shape.getBounds
    val width = insets.left + insets.right + bounds.width
    val height = insets.bottom + insets.top + bounds.height
    new Dimension(width, height)
  }

  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    
//    val rectangle = loc.rectangle
//    g.setColor(Color.BLACK)
//    g.draw(new Rectangle2D.Double(rectangle.x, rectangle.y, rectangle.width, rectangle.height))
    
    val bounds = shape.getBounds
    val insets = peer.getInsets
    
    g.translate(insets.left - bounds.x, insets.top - bounds.y)

    g.fill(shape)

//    if (key.toInt > 15) g.setColor(Color.WHITE) else g.setColor(Color.BLACK)
//
//    val tagIndex = loc.tags.lastIndexOf(" ")
//    val message = {
//      if (tagIndex == -1) loc.tags
//      else loc.tags.substring(tagIndex, loc.tags.length)
//    }
//
//    if (rectangle.width >= 75) g.drawString(message.toString, rectangle.x.toInt, (rectangle.y + rectangle.height / 2).toInt)
//    if (loc.selected) {
//      g.setColor(Color.RED)
//      g.fillOval(rectangle.x.toInt, rectangle.y.toInt, 8, 8)
//    }

    g.dispose()
  }

  def contains(x: Int, y: Int) = {
    val bounds = shape.getBounds
    val insets = peer.getInsets

    val translateX = x + bounds.x - insets.left
    val translateY = y + bounds.y - insets.top

    shape.contains(translateX, translateY)
  }
  
  
  

}
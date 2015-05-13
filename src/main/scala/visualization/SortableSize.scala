package visualization

class SortableSize(var width: Double, var height: Double, var id: Int) {
  
  def setWidth(w : Double) = {
    width = w
  }
  
  def setHeight(h: Double) = {
    height = h
  }
  
  def setId(i: Int) = {
    id = i
  }
  
}
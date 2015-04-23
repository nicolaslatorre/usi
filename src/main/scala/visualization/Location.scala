package visualization

import multidimensionalscaling.Document
import java.util.Date

case class Location(val tags: String, val ids: Map[Int, Date], val count: Int, val rectangle: Rectangle, var selected: Boolean) {

}
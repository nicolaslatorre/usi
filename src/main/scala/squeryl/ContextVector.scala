package squeryl

import org.squeryl.annotations.Column

/**
 * @author nicolaslatorre
 */
class ContextVector(
                      @Column("id")
                      var id: Int,
                      
                      @Column("x")
                      var x: Double,
                      
                      @Column("y")
                      var y: Double) {
  
  def this() = {
    this(-1, -1.0, -1.0)
  }
  
}
package squeryl

import org.squeryl.annotations.Column

/**
 * @author nicolaslatorre
 */
class IndexVector(
                      @Column("term")
                      var term: String,
                      
                      @Column("x")
                      var x: Double,
                      
                      @Column("y")
                      var y: Double) {
  
  def this() = {
    this("", -1.0, -1.0)
  }
  
}
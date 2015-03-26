package squeryl

import org.squeryl.annotations.Column

class Dictionary(
                  @Column("term")
                  var term: String) {
  
  def this() {
    this("")
  }
}
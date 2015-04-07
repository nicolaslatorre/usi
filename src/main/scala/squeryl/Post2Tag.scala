package squeryl

import org.squeryl.annotations._


class Post2Tag(
                @Column("post_id")
                var id: Int,
                
                @Column("tag")
                var tags: String) {
  
  def this() = {
    this(-1, "")
  }

}
package squeryl

import org.squeryl.annotations._
import java.util.Date


case class Comment(
                    @Column("id")
                    var id: Int,

                    @Column("post_id")
                    var postId: Int,

                    @Column("score")
                    var score: Option[Int],

                    @Column("text")
                    var text: String,

                    @Column("creation_date")
                    var creationDate: Date,

                    @Column("user_id")
                    var userId: Option[Int]) {

  def this() = this(-1, -1, Some(0), "", new Date(), Some(-1))

}
package squeryl

import org.squeryl.annotations._
import java.util.Date

case class Vote(
                @Column("id")
                var id : Int,

                @Column("post_id")
                var postId : Int,

                @Column("vote_type_id")
                var voteTypeId : Int,

                @Column("creation_date")
                var creationDate: Date){

  def this() = this(-1,-1,-1,new Date());

}

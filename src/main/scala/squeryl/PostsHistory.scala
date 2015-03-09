package squeryl

/**
 * PostsHistory
 *
 * Created by ponza on 11/25/13, 10:22 AM
 * Copyright (c) 2012 University of Lugano. All right reserved.
 */

import org.squeryl.annotations.Column
import java.util.Date


case class PostsHistory(
                          @Column("id")
                          var id: Int,

                          @Column("post_history_type_id")
                          var postHistoryTypeId: Int,

                          @Column("revision_guid")
                          var revisionGUID: Option[String],

                          @Column("post_id")
                          var postId: Int,

                          @Column("creation_date")
                          var creationDate: Date,

                          @Column("user_id")
                          var userId: Option[Int],

                          @Column("user_display_name")
                          var userDisplayName: Option[String],

                          @Column("comment")
                          var comment : Option[String],

                          @Column("text")
                          var text : Option[String]) {



  def this() {
    this(-1, -1, Some(""), -1, new Date(), Some(-1), Some(""), Some(""), Some("") )
  }

}

package squeryl

import org.squeryl.annotations.Column
import java.util.Date


class BadPosts extends Post() {}
class GoodPosts extends Post() {}

 class Post(
                 @Column("id")
                 var id: Int,

                 @Column("post_type_id")
                 var postTypeId: Int,

                 @Column("parent_id")
                 var parentId: Option[Int],

                 @Column("accepted_answer_id")
                 var acceptedAnswerId: Option[Int],

                 @Column("creation_date")
                 var creationDate: Date,

                 @Column("score")
                 var score: Int,

                 @Column("view_count")
                 var viewCount: Int,

                 @Column("body")
                 var body: String,

                 @Column("owner_user_id")
                 var ownerId: Option[Int],

                 @Column("last_editor_user_id")
                 var lastEditorId: Option[Int],

                 @Column("last_editor_display_name")
                 var lastEditorDisplayName: Option[String],

                 @Column("last_edit_date")
                 var lastEditDate: Option[Date],

                 @Column("last_activity_date")
                 var lastActivitydDate: Option[Date],

                 @Column("community_owned_date")
                 var communityOwnedDate: Option[Date],

                 @Column("closed_date")
                 var closedDate: Option[Date],

                 @Column("title")
                 var title: Option[String],

                 @Column("tags")
                 var tags: Option[String],

                 @Column("answer_count")
                 var answerCount: Option[Int],

                 @Column("comment_count")
                 var commmentCount: Option[Int],

                 @Column("favorite_count")
                 var favoriteCount: Option[Int]) {


  def this() {
    this(-1, -1, Some(-1), Some(-1), new Date(), 0, 0, "", Some(-1), Some(-1), Some(""), Some(new Date()), Some(new Date()), Some(new Date()),
      Some(new Date()), Some(""), Some(""), Some(0), Some(0), Some(0))
  }

}

package squeryl

import org.squeryl.annotations._
import java.util.Date


case class User(
                 @Column("id")
                 var id: Int,

                 @Column("reputation")
                 var reputation: Int,

                 @Column("display_name")
                 var displayName: String,

                 @Column("email_hash")
                 var emailHash: Option[String],

                 @Column("last_access_date")
                 var lastAccessDate: Date,

                 @Column("website_url")
                 var websiteUrl: Option[String],

                 @Column("location")
                 var location: Option[String],

                 @Column("age")
                 var age: Option[Int],

                 @Column("about_me")
                 var aboutMe: Option[String],

                 @Column("views")
                 var views: Int,

                 @Column("up_votes")
                 var upVotes: Int,

                 @Column("down_votes")
                 var downVotes: Int,

                 @Column("creation_date")
                 var creationDate: Date) {

  def this() = this(-2, -1, "", Some(""), new Date, Some(""), Some(""), Some(0), Some(""), 0, 0, 0, new Date())

}
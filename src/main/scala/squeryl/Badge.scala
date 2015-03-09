package squeryl

import org.squeryl.annotations._
import java.util.Date

/**
 * Badge
 *
 * Created by ponza on 11/28/13, 10:41 AM
 * Copyright (c) 2012 University of Lugano. All right reserved.
 */
case class Badge (
            @Column("id")
            var id: Int,

            @Column("user_id")
            var userId : Int,

            @Column("name")
            var name: String,

            @Column("date")
            var date: Date
          ){

  def this() = this(-1,-1,"",new Date())

}


package database

import org.squeryl.PrimitiveTypeMode._
import scala.util.Random

object TagFactory {

  def populateTags(url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    inTransaction {
      val ids = DatabaseRequest.retrieveQuestionsIds()
      println(ids.size + " ids retrieved")

      val chunks = ids.grouped(20000).toList

      val tags = chunks.par.flatMap { chunk =>
        DatabaseRequest.retrieveTagsPosts(chunk)
      }.toMap.seq

      println("retrieved tags")

      DatabaseRequest.insertTags(tags)

    }

    cpds.close()
  }

  def mainTagVector(url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val vector = inTransaction {

      val post2tag = DatabaseRequest.retrieveTag2PostWithDate().map { case (pt, date) => (pt.id, (pt.tags.split(" ").toList, date)) }.toMap

      val v1 = post2tag.groupBy { case (x, (y, z)) => y.take(1) }.mapValues { x => x.map { case (id, (tags, date)) => (id, date) } }.toList
      val v2 = post2tag.filter { case (x, (y, z)) => y.size > 1 }.groupBy { case (x, (y, z)) => y.take(2) }.mapValues { x => x.map { case (id, (tags, date)) => (id, date) } }.toList
      val v3 = post2tag.filter { case (x, (y, z)) => y.size > 2 }.groupBy { case (x, (y, z)) => y.take(3) }.mapValues { x => x.map { case (id, (tags, date)) => (id, date) } }.toList
      val v4 = post2tag.filter { case (x, (y, z)) => y.size > 3 }.groupBy { case (x, (y, z)) => y.take(4) }.mapValues { x => x.map { case (id, (tags, date)) => (id, date) } }.toList
      val v5 = post2tag.filter { case (x, (y, z)) => y.size > 4 }.groupBy { case (x, (y, z)) => y.take(5) }.mapValues { x => x.map { case (id, (tags, date)) => (id, date) } }.toList
      println("Retrieved post2tag")
      List(v1, v2, v3, v4, v5).flatMap { x => x }
    }.toList

    val mainVector = vector.toList.sortBy { case (x, y) => y.size }.reverse

    cpds.close()
    println("Main vector created")
    val tags = mainVector.par.map { case (tags, idsAndDates) => new Tag(tags, idsAndDates, idsAndDates.size) }
    println("Vector length: " + tags.size)
    tags.toList
  }
}
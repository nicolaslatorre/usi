package database

import org.squeryl.PrimitiveTypeMode._
import scala.util.Random

object TagFactory {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    val tagVector = mainTagVector(url, username, password)
  }

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
    mainVector.map { case (tags, x) => new Tag(tags, x, x.size) }
  }

  def buildVectorTag(vector: List[(String, Int)], url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val vectors = inTransaction {
      val post2tag = DatabaseRequest.retrieveTag2Post()
      post2tag.map { case (id, tags) => (id, tags.map { tag => vector.indexOf(tag) }) }
    }.toList

    cpds.close()
    vectors
  }

  def buildVectorTag(postTags: List[String], tagList: List[String]) = {
    val vectorsTag = postTags.map { tag =>
      val index = tagList.indexOf(tag.trim)
      if (index == -1) {
        //System.err.println("index: " + index)
        0
      }
      val end = (tagList.size - 1) - index
      if (index == 0) List(1) ::: List.fill(end - 1)(0)
      else Stream.continually(0).take(index).toList ::: List(1) ::: List.fill(end)(0)
    }

    val vectorTag = vectorsTag.foldLeft(List.fill(tagList.size)(0))((x, y) => x zip y map {
      case (a, b) =>
        if (a >= b) a
        else b
    })
    vectorTag.zipWithIndex.filter { case (x, y) => x > 0 }
  }

}
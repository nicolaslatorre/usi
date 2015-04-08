package database

import org.squeryl.PrimitiveTypeMode._

object TagFactory {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    //    populateTags(url, username, password)
    val tagVector = mainTagVector(url, username, password)
    println(tagVector)
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
      val post2tag = DatabaseRequest.retrieveTag2Post()
      val v = post2tag.groupBy { case (x, y) => y }.mapValues { x => x.toList.size }
      println("Retrieved post2tag")
      v
    }.map { case (x, y) => (x.mkString(" "), y) }

    val mainVector = vector.toList.sortBy { case (x, y) => y }.reverse

    cpds.close()
    println("Main vector created")
    mainVector
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

}
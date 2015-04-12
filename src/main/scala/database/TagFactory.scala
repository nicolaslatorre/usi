package database

import org.squeryl.PrimitiveTypeMode._
import scala.util.Random

object TagFactory {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    //    populateTags(url, username, password)
    val tagVector = mainTagVector(url, username, password)
    //println(tagVector)
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
      //      val v = post2tag.groupBy { case (x, y) => y }.mapValues { x => x.toList.size }
      val v1 = post2tag.groupBy { case (x, y) => y.take(1) }.mapValues { x => x.toList.size }.toList
      val v2 = post2tag.filter { case (x, y) => y.size > 1 }.groupBy { case (x, y) => y.take(2) }.mapValues { x => x.toList.size }.toList
      val v3 = post2tag.filter { case (x, y) => y.size > 2 }.groupBy { case (x, y) => y.take(3) }.mapValues { x => x.toList.size }.toList
      val v4 = post2tag.filter { case (x, y) => y.size > 3 }.groupBy { case (x, y) => y.take(4) }.mapValues { x => x.toList.size }.toList
      val v5 = post2tag.filter { case (x, y) => y.size > 4 }.groupBy { case (x, y) => y.take(5) }.mapValues { x => x.toList.size }.toList
      println("Retrieved post2tag")
      List(v1, v2, v3, v4, v5).flatMap { x => x }
    }.toList

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

  def createVectors(vector: List[Node]) = {
    val s = vector.tail.map { node => node.occurrences }.sum
    println(s)
    val random = new Random
    val vt = vector.map { node =>
      val index = vector.indexOf(node)
      val grade = 1//{
//        if (node.occurrences >= 1 && node.occurrences < 100) node.occurrences
//        else if(node.occurrences >= 100 && node.occurrences < 1000) node.occurrences / 10
//        else if(node.occurrences >= 1000 && node.occurrences < 10000) node.occurrences / 100
//        else if(node.occurrences >= 10000 && node.occurrences < 100000) node.occurrences / 1000
//        else node.occurrences
//      }
      val end = (vector.size - 1) - index
      if (index == 0) List(grade) ::: List.fill(end - 1)(0)
      else Stream.continually(0).take(index).toList ::: List(grade) ::: List.fill(end)(0)
    }
    
    val vectorTag = vt.foldLeft(List.fill(vector.size)(0))((x, y) => x zip y map {
      case (a, b) =>
        if (a >= b) a
        else b
    })
    vectorTag.zipWithIndex.filter { case (x, y) => x > 0 }

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
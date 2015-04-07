package database

import org.squeryl.PrimitiveTypeMode._

object TagFactory {

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"

    populateTags(url, username, password)
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

}
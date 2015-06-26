package database

import java.io.File
import org.jsoup.Jsoup
import com.github.nscala_time.time.Imports.LocalDate
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter

object DataManagement {

//  def main(args: Array[String]) = {
//    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
//    val username = "sodb"
//    val password = "sodb"
//
//    buildDiscussionsFiles(url, username, password)
//    println("END")
//  }

  def buildDiscussionsFiles(url: String, username: String, password: String) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)

    val discussions = DatabaseRequest.retrieveQuestionsInfo()
    cpds.close()

    val singleTags2infos = discussions.groupBy { discussion => discussion.tags.take(1) }

    singleTags2infos.par.map {
      case (single, values) =>
        val savePath = "Discussions/" + single.mkString("") + ".csv"
        val file = new File(savePath)
        file.getParentFile.mkdirs()
        val tags2infos = values.groupBy { value => value.tags }

        tags2infos.map {
          case (tag, ds) =>
            val infos = ds.map { d => d.getInfo() }

            val writer = CSVWriter.open(file, append = true)
            writer.writeAll(infos)
            writer.close()
        }
    }
  }

  def openDiscussionsFiles(tags: List[String], ids: Set[Int]) = {
    val single = tags.take(1).mkString("")
    val path = "../Discussions/" + single + ".csv"
    val file = new java.io.File(path)
    val reader = CSVReader.open(file)

    val lines = reader.iterator
    val discussions = lines.filter { line =>
      val id = line.head.toInt
      ids.contains(id)
    }.toList

    discussions.map { discussion =>
      val Array(id, title, creation, answers, score, view, owner) = discussion.toArray
      val Array(year, month, day) = creation.split("-")
      val creationDate = new LocalDate(year.toInt, month.toInt, day.toInt)

      new Discussion(id.toInt, title, creationDate.toDate(), answers.toInt, score.toInt, view.toInt, owner.toInt, tags)
    }
  }

  def getClosedDate(date: String) = {
    if (date == "Still Open") None
    else {
      val Array(year, month, day) = date.split("-")
      val closed = new LocalDate(year.toInt, month.toInt, day.toInt)
      Some(closed.toDate)
    }
  }

  /**
   * Build tags as a list of strings
   */
  def buildTags(tags: String) = {
    parseHtml(tags).replace("<", "").replace(">", " ").split(" ").toList
  }

  def parseHtml(text: String) = {
    Jsoup.parse(text).body().text()
  }
}
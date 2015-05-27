package database

import org.squeryl.PrimitiveTypeMode._
import scala.util.Random
import com.github.nscala_time.time.Imports.LocalDate
import com.github.tototoshi.csv.CSVWriter
import java.io.File
import org.joda.time.Months
import com.github.nscala_time.time.Imports._
import java.util.Date
import com.github.tototoshi.csv.CSVReader
import java.io.PrintWriter
import visualization.Life

object TagFactory {
  type Frequency = Map[Date, Int]

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val savePath = "../NewDayCounts1000000.csv"

    val start = new LocalDate(2008, 7, 31) //.withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8) //.withDayOfMonth(1)
    val interval = 1
    val life = new Life(start, end, interval)

    val date2step = life.getDateMapping()

    val cpds = DatabaseRequest.openConnection(url, username, password)
    val levels = 5
    val vectors = getVectorsFromTag(url, username, password, levels, date2step, interval)

    cpds.close()

    val file = new File(savePath)
    val writer = CSVWriter.open(file)
    val tags = vectors.map {
      case ((id, tags, title), date) =>
        id.toString :: tags.mkString(" ") :: title :: date.toString() :: Nil
    }.toList

    println("final size: " + tags.size)

    writer.writeAll(tags)
    println("Written")
    writer.close
  }
  
  def getVectorsFromTag(url: String, username: String, password: String, levels: Int, date2step: Map[LocalDate, Int], interval: Int) = {
    val ls = (0 until levels).toList
    val start = new LocalDate(2008, 7, 31)
    val end = new LocalDate(2015, 3, 8)
    val life = new Life(start, end, interval)

    val vectors = inTransaction {
      val post2tag = DatabaseRequest.retrieveTag2PostWithDate(0, 1000000).map { case (pt, title, date) => ((pt.id, pt.tags.split(" ").toList, title.getOrElse("")), date) }.toMap
      println("posts: " + post2tag.size)
      post2tag
    }
    println("Retrieved post2tag")
    vectors
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

  def writeMainFile(url: String, username: String, password: String, life: Life, path: String) = {

  }

  def mainTagVector(url: String, username: String, password: String, life: Life) = {
    val path = "../dayCounts1000000.csv"
    val file = new File(path)
    val iterator = CSVReader.open(file).iterator.grouped(10000)
    println("Opened file")

//    val cpds = DatabaseRequest.openConnection(url, username, password)

    val date2step = life.getStepsMapping()
//    val d2s = life.getDateMapping()
//
//    val post2tag = DatabaseRequest.retrieveTag2PostWithDate(0, 9000000).groupBy {
//      case (p2t, date) =>
//        p2t.tags
//    }.mapValues { result =>
//      result.groupBy {
//        case (p2t, date) =>
//          date.toLocalDate
//      }.mapValues { res => res.map { case (p2t, date) => p2t.id }.toSet }
//    }
//
//    println("Retrieved")
//    cpds.close()

    val reader = iterator.flatMap { lines =>
      lines.par.map { line =>
        val firstCell = line.head
        val tags = firstCell.split(" ").toList

        val days2counts = line.tail.map {
          cell =>
            val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them to the desired interval
            step.toInt -> count.toInt
        }.toList

        val counts = days2counts.map {
          case (step, count) =>
            val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
            index -> count
        }.groupBy { case (step, count) => step }.mapValues { lineCounts =>
          lineCounts.map { case (step, count) => count }.sum
        }

        val date2counts = counts.map {
          case (step, count) =>
            life.incrementByInterval(step) -> count
        }.toMap.seq

//        val ids = post2tag.get(firstCell).getOrElse(Map())
//
//        val dates2ids = ids.map {
//          case (date, ids) =>
//            val index = d2s.getOrElse(date, 0)
//            index -> ids
//        }.groupBy { case (step, count) => life.incrementByInterval(step) }.mapValues { lineCounts =>
//          lineCounts.flatMap { case (step, ids) => ids }.toSet
//        }

        new Tag(tags, date2counts.values.sum, date2counts, days2counts, Map(), Map())
      }
    }.toList

    //    val reader = lines.par.map { line =>
    //      val firstCell = line.head
    //      val tags = firstCell.split(" ").toList
    //
    //      val days2counts = line.tail.map {
    //        cell =>
    //          val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them to the desired interval
    //          step.toInt -> count.toInt
    //      }.toList
    //
    //      val counts = days2counts.map {
    //        case (step, count) =>
    //          val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
    //          index -> count
    //      }.groupBy { case (step, count) => step }.mapValues { lineCounts =>
    //        lineCounts.map { case (step, count) => count }.sum
    //      }
    //
    //      val date2counts = counts.map {
    //        case (step, count) =>
    //          life.incrementByInterval(step) -> count
    //      }.toMap
    //
    //      new Tag(tags, date2counts.values.sum, date2counts, days2counts)
    //    }.toList

    val tags = reader.sortBy { tag => tag.dates2counts.values.max }.reverse
    println("Main vector created, vector length: " + tags.size)
    tags
  }
}
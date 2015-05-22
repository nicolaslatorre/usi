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
    val savePath = "../dayCounts1000000.csv"

    val start = new LocalDate(2008, 7, 31) //.withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8) //.withDayOfMonth(1)
    val interval = 1
    val life = new Life(start, end, interval)

    val date2step = life.getStepsMapping()

    val cpds = DatabaseRequest.openConnection(url, username, password)
    val levels = 5
    val vectors = getVectorsFromTag(url, username, password, levels, date2step, interval)

    cpds.close()

    val file = new File(savePath)
    val writer = CSVWriter.open(file)
    val tags = vectors.map {
      case (tags, count) =>
        tags.mkString(" ") :: count.toList.sortBy { case (date, count) => date }.map { case (d, c) => d + " " + c }
    }

    println("final size: " + tags.size)

    writer.writeAll(tags)
    println("Written")
    writer.close
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

  def mainTagVector(url: String, username: String, password: String, life: Life, date2step: Map[LocalDate, Int]) = {
    val levels = 5
    val path = "../dayCounts.csv"
    val file = new File(path)

    //    val lines = CSVReader.open(file).iterator.grouped(500000)
    val iterator = CSVReader.open(file).iterator.grouped(1000000)
    println("Opened File")

    val reader = iterator.flatMap { lines =>
      lines.par.map { line =>
        val firstCell = line.head
        val tags = firstCell.split(" ").toList

        val counts = line.tail.map {
          cell =>
            val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them to the desired interval
            val date = life.increment(step.toInt)
            val index = date2step.get(date).getOrElse(0) // retrieve actual step
            index -> count.toInt
        }.groupBy { case (step, count) => step }.mapValues { counts =>
          val c = counts.map { case (step, count) => count }.sum
          c
        }.map {
          case (step, count) =>
            life.increment(step * life.interval) -> count
        }.toMap
        tags -> counts
      }
    }.toMap//.foldLeft(Map[List[String], Map[LocalDate, Int]]())((m1, m2) => m1 ++ m2)

    // For some mistery reasons, lines.iterator.toList at this point, speed up the whole computation dramatically.
    // If we put it in the declaration of the variable lines, it is slower.
//    val reader = iterator.toList.par.map { line =>
//      val firstCell = line.head
//      val tags = firstCell.split(" ").toList
//
//      val counts = line.tail.map {
//        cell =>
//          val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them
//          val date = life.increment(step.toInt)
//          val key = date2step.get(date).getOrElse(0) // retrieve actual step
//          key -> count.toInt
//      }.groupBy { case (step, count) => step }.mapValues { counts =>
//        val c = counts.map { case (step, count) => count }.sum
//        c
//      }.map {
//        case (step, count) =>
//          life.increment(step * life.interval) -> count
//      }.toList.toMap
//      tags -> counts
//    }.toMap

    val mainVector = reader.toList.sortBy { case (x, y) => y.values.max }.reverse
    println("Main vector created")

    val tags = mainVector.par.map { case (tags, datesAndIds) => new Tag(tags, datesAndIds, datesAndIds.values.sum, life.interval, reader.get(tags).get) }
    println("Vector length: " + tags.size)
    tags.toList
  }

  def getVectorsFromTag(url: String, username: String, password: String, levels: Int, date2step: Map[LocalDate, Int], interval: Int) = {
    val ls = (0 until levels).toList
    val start = new LocalDate(2008, 7, 31)
    val end = new LocalDate(2015, 3, 8)
    val life = new Life(start, end, interval)

    val vectors = inTransaction {
      val post2tag = DatabaseRequest.retrieveTag2PostWithDate(0, 1000000).map { case (pt, date) => (pt.id, (pt.tags.split(" ").toList, date)) }.toMap
      println("posts: " + post2tag.size)
      ls.par.map { level =>
        post2tag.filter {
          case (x, (y, z)) =>
            y.size > level

        }.groupBy {
          case (x, (y, z)) =>
            y.take(level + 1)
        }.mapValues { x =>
          val res = x.groupBy { case (id, (tags, date)) => date }.mapValues { values =>
            values.keySet.size
          }.toList //.toList.sortBy{case(date, count) => date} // day occurences

          val stepsCount = res.map {
            case (date, count) =>
              val key = date.toLocalDate
              val step = date2step.get(key).get
              step -> count
          }.groupBy { case (step, count) => step }.mapValues { counts => counts.map { case (step, count) => count }.sum }

          stepsCount
        }.toList
      }
    }
    println("Retrieved post2tag")

    vectors.flatMap { x => x }.toList
  }

  def buildVectorFromDB() = {
    //    val reader = vector.map { line =>
    //      val tags = line._1
    //
    //      val counts = line._2.map {
    //        case (cell) =>
    //          val (step, count) = cell
    //          start.plusDays(step.toInt) -> count.toInt
    //      }.toMap
    //      tags -> counts
    //    }.toMap
  }
}
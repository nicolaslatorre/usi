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
import org.jsoup.Jsoup

object TagFactory {
  type Frequency = Map[Date, Int]

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    //    val savePath = "../NwDayCounts1000000.csv"

    val start = new LocalDate(2008, 7, 31) //.withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8) //.withDayOfMonth(1)
    val interval = 1
    val life = new Life(start, end, interval)

    val date2step = life.getDateMapping()

    val cpds = DatabaseRequest.openConnection(url, username, password)
    val levels = 5
    //    val vectors = getVectorsFromTag(url, username, password, levels, date2step, interval)
    val vectors = getTags(url, username, password, levels, date2step, interval)

    cpds.close()

    println(vectors.size)

    //    val file = new File(savePath)
    //    val writer = CSVWriter.open(file)
    val tags = vectors.map {
      case (tag, tagList) =>

        val lines = tagList.map {
          case (tag, total, infos) =>
            val first = tag.mkString(" ") :: total.toString :: Nil
            first ::: infos
        }.toList
        val savePath = "../NoIdsTags2000000/" + tag.mkString("") + ".csv"
        val file = new File(savePath)
        val writer = CSVWriter.open(file, append = true)
        writer.writeAll(lines)
        //        println("Written")
        writer.close
    }.toList

    println("final size: " + tags.size)

    //    writer.writeAll(tags)
    //    println("Written")
    //    writer.close
  }

  def getVectorsFromTag(url: String, username: String, password: String, levels: Int, date2step: Map[LocalDate, Int], interval: Int) = {
    val ls = (0 until levels).toList
    val start = new LocalDate(2008, 7, 31)
    val end = new LocalDate(2015, 3, 8)
    val life = new Life(start, end, interval)

    val vectors = inTransaction {
      //      DatabaseRequest.retrieveTag2PostWithDate(0, 1000000).par.groupBy {
      //        case (pt, title, date) =>
      //          date
      //      }.mapValues { values =>
      //        values.flatMap {
      //          case (pt, title, date) =>
      //            
      //            pt.id.toString :: pt.tags :: date :: Nil
      //        }
      //      }

      DatabaseRequest.retrieveTag2PostWithDate(0, 1000000).par.map {
        case (pt, date) =>
          (pt.id, pt.tags, date)
      }
    }

    println("Retrieved post2tag")
    vectors
  }

  def getTags(url: String, username: String, password: String, levels: Int, date2step: Map[LocalDate, Int], interval: Int) = {
    val ls = (0 until levels).toList
    val start = new LocalDate(2008, 7, 31)
    val end = new LocalDate(2015, 3, 8)
    val life = new Life(start, end, interval)
    val date2step = life.getDateMapping()

    val vectors = inTransaction {
      val discussions = DatabaseRequest.retrieveTag2PostWithDate(0, 2000000).par.map {
        case (pt, date) =>
          (pt.id, pt.tags.split(" ").toList, date)
      }.toList

      val tagsLevel = ls.par.flatMap { level =>
        discussions.filter {
          case (id, tags, date) =>
            tags.size > level
        }.groupBy {
          case (id, tags, date) =>
            tags.take(level + 1)
        }
      }.toMap

      val tags = tagsLevel.map {
        case (tag, values) =>
          val date2discussions = values.groupBy { case (id, tags, date) => date }.toMap.seq
          val totalCount = date2discussions.map { case (date, ids) => ids.size }.sum
          //          val ids = date2discussions.toList.sortBy {
          //            case (date, ids) =>
          //              date
          //          }.map {
          //            case (date, ids) =>
          //              date.toString() + " " + ids.size + " " + ids.filter {
          //                case (id, ts, date) =>
          //                  tag == ts
          //              }.map { case (id, tags, date) => id }.mkString(" ")
          //          }

          val ids = date2discussions.toList.sortBy {
            case (date, ids) =>
              date
          }.map {
            case (date, ids) =>
              date.toString() + " " + ids.size
          }

          (tag, totalCount, ids)
      }.seq.groupBy { case (tag, total, values) => tag.take(1) }
      tags
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

  def createVectorFromTags(url: String, username: String, password: String, life: Life, levels: Int) = {
    val ls = (0 until levels).toList
    val path = "../NewTags2000000"

    val files = new java.io.File(path).listFiles.filter(_.getName.endsWith(".csv")).toSet
    val date2step = life.getDateMapping()

    println("files: " + files.size)

    val chunks = files.grouped(1000).toSet

    val tss = chunks.par.flatMap { files =>
      val ts = files.map { file =>
        val reader = CSVReader.open(file)
        val linesStream = reader.iterator

        val tags = linesStream.map { line =>

          val tags = line.head.split(" ").toList
          val total = line(1).toInt
          val infos = line.drop(2).map { element =>
            val steps = element.split(" ").toList
            val Array(year, month, day) = steps.head.split("-")
            val date = new LocalDate(year.toInt, month.toInt, day.toInt)
            val count = steps(1)
            
            val ids = steps.drop(2).map { x => x.toInt }

            (date, count, ids)
          }.toList

          val dates2ids = infos.map {
            case (date, count, ids) =>
              (date, (count.toInt, ids.toStream))
          }

          new Tag(tags, total, dates2ids.toMap)

        }.toList
        reader.close()
        
        val sortedTags = tags.sortBy { tag => tag.tags.size }
        TagTree.createTree(sortedTags)
      }.toStream
      ts
    }.toList

    println("Main vector created, vector length: " + tss.size)
    val sortedTree = tss.filter { tree => tree.value.tags.size == 1 }.sortBy { tree => tree.getMaxDayCount() }.reverse
    val total = tss.view.filter { tree => tree.value.tags.size == 1 }.map { tree => tree.value.total }.sum
    MTree(new Tag(List(), total, Map()), sortedTree)
  }
}
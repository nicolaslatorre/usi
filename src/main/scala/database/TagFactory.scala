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

    //    val file = new File(savePath)
    //    val writer = CSVWriter.open(file)
    val tags = vectors.map {
      case (tags, infos) =>
        val lines = infos.map {
          case (id, ts, date) =>
            id.toString :: ts :: date.toString() :: Nil
        }.toList

        val savePath = "../Tags8000000/" + tags.mkString("") + ".csv"
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

      DatabaseRequest.retrieveTag2PostWithDate(7000000, 1000000).par.map {
        case (pt, date) =>
          (pt.id, pt.tags, date)
      }.groupBy { case (id, tags, date) => tags.split(" ").toList.take(1) }
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

  def createVectorFromTags(url: String, username: String, password: String, life: Life, levels: Int, dataset: Int) = {
    val ls = (0 until levels).toList
    val path = "../Tags" + ((dataset * 1000000) + 1000000) 

//        val files = new java.io.File(path).listFiles.filter( x => x.length() > 1048576).toSet // 1.50 ~
    //    val files = new java.io.File(path).listFiles.filter( x => x.length() > 1048576 * 60).toList.sortBy { x => x.length() } // 1.10
//    val files = new java.io.File(path).listFiles.filter(x => x.length() < 1048576).toSet // 23 seconds
    //    val files = new java.io.File(path).listFiles.filter( x => (x.length() > 1048576) && (x.length() < 1048576 * 60)).toSet//.toList.sortBy { x => x.length() }.reverse // 1.14
            val files = new java.io.File(path).listFiles.filter(_.getName.endsWith(".csv")).toSet
    val date2step = life.getDateMapping()

    println("files: " + files.size)
    val chunks = files.grouped(10).toSet

    val tss = chunks.par.flatMap { files =>

      val ts = files.map { file =>
        val reader = CSVReader.open(file)
        val linesStream = reader.iterator

        val discussions = linesStream.map { line =>
          val id = line.head.toInt
          val tags = line(1).split(" ").toList
          val Array(year, month, day) = line(2).split("-")
          val date = new LocalDate(year.toInt, month.toInt, day.toInt)

          (id, tags, date)
        }.toSet

        //        println("Discussions: ")

        reader.close()
        

        val tagsLevel = ls.flatMap { level =>
          discussions.filter {
            case (id, tags, date) =>
              tags.size > level
          }.groupBy {
            case (id, tags, date) =>
              tags.take(level + 1)
          }
        }.toMap

        //        println("Levels")

        val tags = tagsLevel.map {
          case (tags, ds) =>
            val infos = ds.groupBy { case (id, tags, date) => date }.toMap

            val days2counts = infos.map { case (date, ids) => (date, ids.size) }
            val totalCount = infos.map { case (date, ids) => ids.size }.sum
            val dates2ids = infos.map {
              case (date, ids) =>
                val levelIds = ids.filter { case (id, ts, date) => tags == ts }.map { case (id, ts, date) => id }
                (date, levelIds.toSet.seq)
            }
            //
//            val dates2counts = days2counts.par.groupBy {
//              case (step, count) =>
//                val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
//                life.incrementByInterval(index)
//            }.mapValues { values =>
//              values.map { case (step, count) => count }.sum
//            }
//
//            val d2i = dates2ids.par.groupBy {
//              case (date, count) =>
//                val index = date2step.get(date).getOrElse(0) // retrieve actual step index respect to the interval
//                life.incrementByInterval(index)
//            }.mapValues { values =>
//              //            values.toMap.values
//              values.flatMap { case (date, ids) => ids }.toSet.seq
//            }

            //          new Tag(tags, totalCount, dates2counts.toMap.seq, days2counts.toList, dates2ids.toMap.seq, d2i.toMap.seq)
            new Tag(tags, totalCount, days2counts.seq, days2counts.toList, dates2ids.seq, dates2ids.seq)
        }

        //        println("Tags")

        val sortedTags = tags.toList.sortBy { tag => tag.tags.size }
        val subtrees = TagTree.createSubTree(sortedTags.tail, 1)
        val tree = new Tree(new Node(sortedTags.head, subtrees))

        //        println("Tree")
        tree
      }.toList
      ts
    }.toList

    println("Main vector created, vector length: " + tss.size)
    val subtrees = tss.map { tree => tree.root }.sortBy { root => root.tag.getMaxIntervalCount() }.reverse
    val totalDiscussions = tss.view.filter { tree => tree.root.tag.tags.size == 1 }.map { tree => tree.root.tag.totalCount }.sum
    new Tree(new Node(new Tag(List(), totalDiscussions, Map(), List(), Map(), Map()), subtrees))
    //    ts.sortBy { tag => tag.dates2counts.values.max }.reverse
  }

  def mainTagVector(url: String, username: String, password: String, life: Life, levels: Int) = {
    val path = "../dayCounts.csv"
    val file = new File(path)
    val iterator = CSVReader.open(file).iterator.grouped(1000000)
    println("Opened file")

    val date2step = life.getDateMapping()

    val reader = iterator.flatMap { lines =>
      lines.par.map { line =>
        val firstCell = line.head
        val tags = firstCell.split(" ").toList

        val days2counts = line.tail.map {
          cell =>
            val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them to the desired interval
            life.increment(step.toInt) -> count.toInt
        }.toList

        val dates2counts = days2counts.groupBy {
          case (step, count) =>
            val index = date2step.get(step).getOrElse(0) // retrieve actual step index respect to the interval
            life.incrementByInterval(index)
        }.mapValues { lineCounts =>
          lineCounts.map { case (step, count) => count }.sum
        }

        //        val date2counts = counts.map {
        //          case (step, count) =>
        //            life.incrementByInterval(step) -> count
        //        }.toMap.seq

        //        val ids = post2tag.get(firstCell).getOrElse(Map())
        //
        //        val dates2ids = ids.map {
        //          case (date, ids) =>
        //            val index = d2s.getOrElse(date, 0)
        //            index -> ids
        //        }.groupBy { case (step, count) => life.incrementByInterval(step) }.mapValues { lineCounts =>
        //          lineCounts.flatMap { case (step, ids) => ids }.toSet
        //        }

        new Tag(tags, dates2counts.values.sum, dates2counts, days2counts, Map(), Map())
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

    println("Main vector created, vector length: " + reader.size)
    reader.sortBy { tag => tag.dates2counts.values.max }.reverse
    //    tags
  }
}
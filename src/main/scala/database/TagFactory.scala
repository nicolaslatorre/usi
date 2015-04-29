package database

import org.squeryl.PrimitiveTypeMode._
import scala.util.Random
import org.joda.time.LocalDate
import com.github.tototoshi.csv.CSVWriter
import java.io.File
import org.joda.time.Months
import com.github.nscala_time.time.Imports._
import java.util.Date
import com.github.tototoshi.csv.CSVReader
import java.io.PrintWriter

object TagFactory {
  type Frequency = Map[Date, Int]

  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val interval = 1
    val savePath = "../MonthCountsNext100000.csv"

//    val singleVector = getVectors(url, username, password, 1).map { case (tags, mapping) => tags.mkString(" ") }
//    println("single vector size: " + singleVector.size)

    val start = new LocalDate(2008, 7, 31).withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8).withDayOfMonth(1)
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths).toList
    val dates2month = ms.map { month => start.plusMonths(month) -> month }.toMap

    val cpds = DatabaseRequest.openConnection(url, username, password)
    val levels = 5
//    val vectors = singleVector.par.map { tag => getVectorsFromTag(url, username, password, levels, tag, dates2month) }.toList
    val vectors = getVectorsFromTag(url, username, password, levels, "", dates2month)

    cpds.close()

    val file = new File(savePath)
    val writer = CSVWriter.open(file)
    val tags = vectors.map {
      case (tags, count) =>
//        val newline = line.map { case (tags, count) => tags.mkString(" ") :: count.toList.sortBy { case (date, count) => date }.map { case (d, c) => d + " " + c } }
//        newline
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

  def generateFileWithMonthCount(url: String, username: String, password: String, iteration: Int, mapping: scala.collection.mutable.Map[String, Map[Int, Int]]) = {
    val page = 50000
    println("current mapping size: " + mapping.size)
    val cpds = DatabaseRequest.openConnection(url, username, password)
    val startDate = new LocalDate(2015, 3, 8).withDayOfMonth(1)

    val vector = inTransaction {

      val post2tag = DatabaseRequest.retrieveTag2PostWithDate(page * iteration, page).map { case (pt, date) => (pt.id, (pt.tags.split(" ").toList, date)) }.toMap

      val v1 = post2tag.groupBy { case (x, (y, z)) => y.take(1) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      val v2 = post2tag.filter { case (x, (y, z)) => y.size > 1 }.groupBy { case (x, (y, z)) => y.take(2) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      val v3 = post2tag.filter { case (x, (y, z)) => y.size > 2 }.groupBy { case (x, (y, z)) => y.take(3) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      val v4 = post2tag.filter { case (x, (y, z)) => y.size > 3 }.groupBy { case (x, (y, z)) => y.take(4) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      val v5 = post2tag.filter { case (x, (y, z)) => y.size > 4 }.groupBy { case (x, (y, z)) => y.take(5) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      println("Retrieved post2tag")
      List(v1, v2, v3, v4, v5).flatMap { x => x }
    }.toList

    val mainVector = vector.toList.sortBy { case (x, y) => y.values.flatMap { x => x }.size }.reverse

    println("Vector Ready, size: " + mainVector.size)

    cpds.close()

    val start = new LocalDate(2008, 7, 31).withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8).withDayOfMonth(1)
    val tags = mainVector.map {
      case (tags, datesAndIds) =>
        val counts = getMonthsCount(start, end, 1, datesAndIds)
        if (!mapping.contains(tags.mkString(" "))) {
          mapping.put(tags.mkString(" "), counts)
        } else {
          val oldCount = mapping.get(tags.mkString(" ")).get
          val newMap = mergeMap(List(oldCount, counts))((v1, v2) => v1 + v2)
          mapping.put(tags.mkString(" "), newMap)
        }
      //      println("processed tag: " + tags.mkString(" "))
    }.toList

    println("count done")

    mapping

  }

  def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] = {
    (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }
  }

  def mainTagVector(url: String, username: String, password: String, interval: Int) = {
    val levels = 5
    val start = new LocalDate(2008, 7, 31).withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8).withDayOfMonth(1)
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths).toList

//    val vector = getVectors(url, username, password, levels)

//    val mainVector = vector.toList.sortBy { case (x, y) => y.values.flatMap { x => x }.size }.reverse

    println("Main vector created")

    val file = new File("../MonthCountsNext100000.csv")
    val reader = CSVReader.open(file).all.map { line =>
      val tags = line.head.split(" ").toList

      val counts = line.tail.map {
        case (cell) =>
          val Array(month, count) = cell.split(" ")
          start.plusMonths(month.toInt) -> count.toInt
      }.toMap
      tags -> counts
    }.toMap
    
    val mainVector = reader.toList.sortBy { case (x, y) => y.values.max }.reverse

    val tags = mainVector.par.map { case (tags, datesAndIds) => new Tag(tags, datesAndIds, datesAndIds.values.sum, interval, reader.get(tags).get) }
    println("Vector length: " + tags.size)
    tags.toList
  }

  def mainTagVectorFromTag(url: String, username: String, password: String, interval: Int, vectors: List[(List[String], Map[Date, Set[Int]])], mapping: scala.collection.mutable.Map[String, Map[Int, Int]]) = {
    val levels = 5

    val mainVector = vectors.toList.sortBy { case (x, y) => y.values.flatMap { x => x }.size }.reverse

    println("Vector Ready, size: " + mainVector.size)

    val start = new LocalDate(2008, 7, 31).withDayOfMonth(1)
    val end = new LocalDate(2015, 3, 8).withDayOfMonth(1)
    val tags = mainVector.par.map {
      case (tags, datesAndIds) =>
        val counts = getMonthsCount(start, end, 1, datesAndIds)
        mapping.put(tags.mkString(" "), counts)
    }.toList

    println("count done")

    mapping
  }

  def getVectors(url: String, username: String, password: String, levels: Int) = {
    val cpds = DatabaseRequest.openConnection(url, username, password)
    val startDate = new LocalDate(2009, 3, 31).withDayOfMonth(1)

    val ls = (0 until levels).toList
    val vectors = inTransaction {
      val post2tag = DatabaseRequest.retrieveTag2PostWithDate(0, 9000000).map { case (pt, date) => (pt.id, (pt.tags.split(" ").toList, date)) }.toMap
      println("posts: " + post2tag.size)
      ls.map { level =>
        post2tag.filter { case (x, (y, z)) => y.size > level }.groupBy { case (x, (y, z)) => y.take(level + 1) }.mapValues { x => x.groupBy { case (id, (tags, date)) => date }.mapValues { x => x.keySet } }.toList
      }
    }
    println("Retrieved post2tag")
    cpds.close
    vectors.flatMap { x => x }
  }

  def getVectorsFromTag(url: String, username: String, password: String, levels: Int, tag: String, dates2month: Map[LocalDate, Int]) = {
    val ls = (0 until levels).toList

    val vectors = inTransaction {
      val post2tag = DatabaseRequest.retrieveTag2PostWithDate(0, 100000).map { case (pt, date) => (pt.id, (pt.tags.split(" ").toList, date)) }.toMap
      println("posts: " + post2tag.size)
      ls.par.map { level =>
        post2tag.filter {
          case (x, (y, z)) =>
            y.size > level

        }.groupBy {
          case (x, (y, z)) =>
            y.take(level + 1)
        }.mapValues { x =>
          val res = x.groupBy { case (id, (tags, date)) => date }.mapValues { a => 
            
            a.keySet.size 
            
          } // day occurences
          val mc = res.groupBy { case (date, count) => date.toLocalDate.getYear }.mapValues {
            y =>
              y.groupBy { case (d, c) => d.toLocalDate.getMonthOfYear }.mapValues {
                z => 
                  z.map{ case(d1, c1) => c1}.sum
              }
          }
          val res1 = mc.flatMap {
            case (year, monthCount) => monthCount.map {
              case (month, count) =>
                val date = new LocalDate(year, month, 1)
                val monthIndex = dates2month.get(date).get
                monthIndex -> count
            }
          }
          res1
        }.toList
      }
    }
    println("Retrieved post2tag")

    vectors.flatMap { x => x }.toList
  }

  /**
   * Generate a map between the month number and the count of that month.
   */
  def getMonthsCount(start: LocalDate, end: LocalDate, interval: Int, ids: Map[Date, Set[Int]]) = {
    val months = Months.monthsBetween(start, end)
    val ms = (0 to months.getMonths)

    ms.filter { month => month % interval == 0 }.map { month => (month, getSingleMonthCount(start.plusMonths(month), ids)) }.toMap.seq
    //    ms.map { month => start.plusMonths(month) -> getAverageMonthCount(start.plusMonths(month)) }.toMap
  }

  def getSingleMonthCount(start: LocalDate, ids: Map[Date, Set[Int]]) = {
    val end = start.plusMonths(1)
    val counts = getIdsInDate(start, end, ids)
    counts.values.flatMap { x => x }.size
    //start -> counts.size // the size can be computed on demand, here it can be useful to have the counts
  }

  /**
   * Check if there are discussions in the given interval of dates
   * @param startDate the start date
   * @param endDate the end date
   * @return The ids and date in the given interval, returns an empty Map if there no ids in the given interval
   */
  def getIdsInDate(startDate: LocalDate, endDate: LocalDate, ids: Map[Date, Set[Int]]) = {
    ids.par.filter {
      case (date, id) =>
        date.toLocalDate >= startDate && date.toLocalDate < endDate
    }.toMap
  }
}
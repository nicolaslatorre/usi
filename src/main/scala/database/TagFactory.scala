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

  def mainTagVector(life: Life) = {
    val path = "../dayCounts1000000.csv"
    val file = new File(path)
    val date2step = life.getStepsMapping()

    val iterator = CSVReader.open(file).iterator.grouped(100000)
    println("Opened File")

    val reader = iterator.flatMap { lines =>
      lines.par.map { line =>
        val firstCell = line.head
        val tags = firstCell.split(" ").toList

        val counts = line.tail.map {
          cell =>
            val Array(step, count) = cell.split(" ") // daily steps, we need to adapt them to the desired interval
            val index = date2step.get(step.toInt).getOrElse(0) // retrieve actual step index respect to the interval
            index -> count.toInt
        }.groupBy { case (step, count) => step }.mapValues { lineCounts =>
          lineCounts.map { case (step, count) => count }.sum
        }
        
        val date2counts = counts.map {
          case (step, count) =>
            life.incrementByInterval(step) -> count
        }.toMap
        
        tags -> date2counts
      }
    }.toList

    val mainVector = reader.sortBy { case (tags, dates2counts) => dates2counts.values.max }.reverse
    val tags = mainVector.par.map { case (tags, dates2counts) => new Tag(tags, dates2counts.values.sum, dates2counts) }
    
    println("Main vector created, vector length: " + tags.size)
    tags.toList
  }
}
package database

import java.io.File
import java.util.Date

import org.squeryl.PrimitiveTypeMode.inTransaction

import com.github.nscala_time.time.Imports.LocalDate
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter

import visualization.Life

object TagFactory {

  def createVectorFromTags(life: Life, levels: Int) = {
    val ls = (0 until levels).toList
    val path = "../NewTags"

    val files = new java.io.File(path).listFiles.filter(_.getName.endsWith(".csv")).toSet
    val date2step = life.getDateMapping()


    val chunks = files.grouped(1000).toSet

    val tss = chunks.par.flatMap { files =>
      val ts = files.map { file =>
        val reader = CSVReader.open(file)
        val linesStream = reader.iterator

        val tags = linesStream /*.filter { line => line.head.split(" ").toList.size == 1 }*/ .map { line =>

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

    println("Tags Created")
    val sortedTree = tss.filter { tree => tree.value.tags.size == 1 }.sortBy { tree => tree.getMaxDayCount() }.reverse
    val total = tss.view.filter { tree => tree.value.tags.size == 1 }.map { tree => tree.value.total }.sum
    MTree(new Tag(List(), total, Map()), sortedTree)
  }

  def updateVectorFromTags(life: Life, tags: List[String], root: MTree) = {
    val single = tags.take(1).mkString("")
    val path = "NewTags/" + single + ".csv"

    val file = new java.io.File(path)
    val date2step = life.getDateMapping()

    val reader = CSVReader.open(file)
    val linesStream = reader.iterator

    val tss = linesStream.filter { line =>
      val length = tags.size
      val current = line.head.split(" ").toList
      val initial = current.take(length)

      (initial == tags && current.size == (tags.size + 1))
    }.map { line =>

      val ts = line.head.split(" ").toList
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

      new Tag(ts, total, dates2ids.toMap)

    }.toList
    reader.close()

    val subtrees = tss.map { t => new MTree(t, Nil) }.sortBy { tree => tree.getMaxDayCount() }.reverse

    println("Main vector created, vector length: " + tss.size)
    subtrees
  }
}
package database

import scala.io.Source
import java.io.File
import org.jsoup.Jsoup
import squeryl.Post
import squeryl.Comment
import com.github.tototoshi.csv.CSVWriter

object TagManager {
  def main(args: Array[String]) = {
    val path = "../tags-recurrence.csv"
    val singleTagsPath = "../Datasets/single-tags-recurrence.csv"
    val tagsOccurences = buildSingleTags(path)

    writeTagsOccurences(tagsOccurences, singleTagsPath)

  }

  def buildSingleTags(path: String) = {
    val tags = Source.fromFile(new File(path)).getLines().map { x =>
      val Array(tag, occurences) = x.split(",")
      (tag, occurences.toInt)
    }.toList

    val singles = tags.flatMap {
      case (x, y) =>
        val tag = x.split(" ").toList
        tag.map { t => (t, y) }
    }.toList //.groupBy(x => x(0)).mapValues { x => x.map { y => y.tail.map { z => z.to }) } }

    val distinctSingles = singles.groupBy(_._1).mapValues { x => x.map { case (x, y) => y } }.map { case (x, y) => (x, y.sum) }.toList

    distinctSingles.sortBy(x => x._2).reverse
  }

  def getVectorTag(postTags: List[String], tagsOccurences: List[(String, Int)]) = {
    val tagList = tagsOccurences.map(x => x._1)

    val vectorsTag = postTags.map { tag =>
      val index = tagList.indexOf(tag.trim)
      if (index == -1) {
        //System.err.println("index: " + index)
        0
      }
      val end = (tagList.size - 1) - index
      if (index == 0) List(1) ::: List.fill(end-1)(0) 
      else Stream.continually(0).take(index).toList ::: List(1) ::: List.fill(end)(0)
    }

    val vectorTag = vectorsTag.foldLeft(List.fill(tagList.size)(0))((x, y) => x zip y map {
      case (a, b) =>
        if (a >= b) a
        else b
    })
    vectorTag
  }
  
  
  def buildVectorTag(postTags: List[String], tagList: List[String]) = {
    val vectorsTag = postTags.map { tag =>
      val index = tagList.indexOf(tag.trim)
      if (index == -1) {
        //System.err.println("index: " + index)
        0
      }
      val end = (tagList.size - 1) - index
      if (index == 0) List(1) ::: List.fill(end-1)(0) 
      else Stream.continually(0).take(index).toList ::: List(1) ::: List.fill(end)(0)
    }

    val vectorTag = vectorsTag.foldLeft(List.fill(tagList.size)(0))((x, y) => x zip y map {
      case (a, b) =>
        if (a >= b) a
        else b
    })
    vectorTag
  }
  
  
//  def getVectorTag(post: Post, tagsOccurences: List[(String, Int)]) = {
//    val postTagsString = post.tags match {
//      case Some(t) => t
//      case None => ""
//    }
//    val postTags = Jsoup.parse(postTagsString).body().text().replace("<", "").replace(">", " ").split(" ").toList
//
//    val tagList = tagsOccurences.map(x => x._1)
//
//    val vectorsTag = postTags.map { tag =>
//      val index = tagList.indexOf(tag.trim)
//      if (index == -1) {
//        System.err.println("index: " + index)
//      }
//      println(index)
//      val end = (tagList.size - 1) - index
//      if (index == 0) List(1) ::: List.fill(end-1)(0) 
//      else Stream.continually(0).take(index).toList ::: List(1) ::: List.fill(end)(0)
//    }
//
//    val vectorTag = vectorsTag.foldLeft(List.fill(tagList.size)(0))((x, y) => x zip y map {
//      case (a, b) =>
//        if (a >= b) a
//        else b
//    })
//
//    println(vectorsTag)
//    vectorTag
//  }

  def writeTagsOccurences(tagsOccurences: List[(String, Int)], path: String) = {
    val content = tagsOccurences.map { case (tag, value) => List(tag, value.toString()) }
    val file = new File(path)
    val writer = CSVWriter.open(file)
    writer.writeAll(content)
    writer.close()
  }

}
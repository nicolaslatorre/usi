package database

import scala.collection.immutable.TreeMap
import java.io.PrintWriter
import java.io.File
import java.util.Date
import visualization.Life
import org.joda.time.LocalDate

object TagTree {

  def createTree(vector: List[Tag]) = {
    val subtrees = createSubTree(vector.tail, 1)
    val tree = new MTree(vector.head, subtrees)
    tree
  }

  def createSubTree(tags: List[Tag], level: Int): List[MTree] = {
    tags match {
      case Nil => Nil
      case x :: Nil => MTree(x, List()) :: Nil
      case x :: xs =>
        val subtrees = tags.par.filter { tag => tag.tags.size > level }.groupBy { v => v.tags.take(level + 1) }.mapValues { tagsInLevel =>
          val sortedTags = tagsInLevel.toList.sortBy { tag => tag.tags.size }
          val subsubtree = createSubTree(sortedTags.tail, level + 1)
          val subtree = new MTree(sortedTags.head, subsubtree)
          
          subtree
        }.values.toList
        subtrees.sortBy { subtree => subtree.getMaxDayCount() }.reverse
    }
  }
}

case class MTree(var value: Tag, var children: List[MTree]) {
  def this(value: Tag) = this(value, List())
  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  def search(key: Tag): MTree = {
    val level = value.tags.length + 1
    val res = children.par.find { child => child.value.tags == key.tags.take(level) }
    res match {
      case Some(n) => 
        n.search(key)
      case None => this
    }
  }
  
  def search(key: List[String]): MTree = {
    val level = value.tags.length + 1
    val res = children.par.find { child => child.value.tags == key.take(level) }
    res match {
      case Some(n) => 
        n.search(key)
      case None => this
    }
  }
  
  def getTotal(date: LocalDate): Int = {
    val count = value.getDayCount(date)
    count + children.map{ child => child.getTotal(date)}.sum
  }
  
  def getCurrentTotal(date: LocalDate): Int = {
    val count = value.getDayCount(date)
    count + children.map{ child => child.getCurrentTotal(date)}.sum
  }
  
  def getMaxDayCount() = {
    value.getMaxDayCount()
  }

  def getLevel() = {
    value :: children.map { child => child.value }
  }

  def getSize(): Int = {
    1 + children.size + children.map { child => child.getSize() }.sum
  }

  def changeCounts(life: Life, date2step: Map[LocalDate, Int]) = {
//    value.changeDates2Counts(life, date2step)
//    children.par.foreach { child =>
//      child.value.changeDates2Counts(life, date2step)
//    }
  }
  
//  def update(tag: Tag) = {
//    val target = search(root, tag.tags)
//    val targetTag = target.tag
//    val previousCount = targetTag.totalCount
//
//    targetTag.totalCount = previousCount + tag.totalCount
//    targetTag.days2counts = tag.days2counts ++ targetTag.days2counts
//    targetTag.dates2counts = tag.dates2counts ++ targetTag.dates2counts
//
//    targetTag.ids = tag.ids ++ targetTag.ids
//    targetTag.dates2ids = tag.dates2ids ++ targetTag.dates2ids
//
//  }

}

object MTree {
  def apply(value: Tag) = new MTree(value, List())
}
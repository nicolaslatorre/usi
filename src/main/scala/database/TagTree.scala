package database

import scala.collection.immutable.TreeMap
import java.io.PrintWriter
import java.io.File
import java.util.Date
import visualization.Life
import org.joda.time.LocalDate

object TagTree {

  def createTree(vector: List[Tag]) = {
    println("Its tree time bitch")
    val total = vector.filter { v => v.tags.size == 1 }.map { v => v.totalCount }.sum

    val subtrees = createSubTree(vector, 0)
    val tree = new Tree(new Node(new Tag(List(), total, Map(), List(), Map(), Map()), subtrees))
    println("Habemus Tree")
    tree
  }

  def createSubTree(tags: List[Tag], level: Int): List[Node] = {
    tags match {
      case Nil => List()
      case x :: Nil => List(new Node(x, List()))
      case x :: xs =>
        val subtrees = tags.par.filter { tag => tag.tags.size > level }.groupBy { v => v.tags.take(level + 1) }.mapValues { tagsInLevel =>
          val sortedTags = tagsInLevel.seq.sortBy { tag => tag.tags.size }

          val subsubtree = createSubTree(sortedTags.toList.tail, level + 1)
          val root = new Node(sortedTags.head, Nil)
          root.children = subsubtree
          val subtree = new Tree(root)
          subtree
        }
        subtrees.values.map { subtree => subtree.root }.toList.sortBy { node => node.tag.getMaxIntervalCount() }.reverse
    }
  }

}

class Tree(val root: Node) {
  def search(current: Node, key: List[String]): Node = {
    val level = current.tag.tags.length + 1
    val res = current.children.find { child => child.tag.tags == key.take(level) }
    res match {
      case Some(n) => search(n, key)
      case None => current

    }
  }

  def getLevel(tags: List[String]) = {
    tags match {
      case Nil => root :: root.children
      case _ =>
        val node = search(root, tags)
        node :: node.children
    }
  }
  
  def update(tag: Tag) = {
    val target = search(root, tag.tags)
    val targetTag = target.tag
    val previousCount = targetTag.totalCount
    
    targetTag.totalCount = previousCount + tag.totalCount
    targetTag.days2counts = tag.days2counts ++ targetTag.days2counts
    targetTag.dates2counts = tag.dates2counts ++ targetTag.dates2counts
    
    targetTag.ids = tag.ids ++ targetTag.ids
    targetTag.dates2ids = tag.dates2ids ++ targetTag.dates2ids
    
    
    
  }

  def getSize(node: Node): Int = {
    val childrens = node.children
    if (childrens.size > 0) {
       childrens.size + childrens.par.map{ child => getSize(child) }.sum
    } else {
      0
    }
  }
  
  def changeCounts(node: Node, life: Life, date2step: Map[LocalDate, Int]): Unit = { 
    val childrens = node.children
    childrens.par.foreach { child => 
      child.tag.changeDates2Counts(life, date2step)
      
      changeCounts(child, life, date2step)
    }
  }

  override def toString() = root.toString()

}

case class Node(val tag: Tag, var children: List[Node]) {
  override def toString() = {
    tag.tags.mkString(" ") + " (" + tag.totalCount + ")\n" + children.map { child => "\t" * tag.tags.length + child.toString() }.mkString("")
  }
}



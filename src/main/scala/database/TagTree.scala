package database

import scala.collection.immutable.TreeMap
import java.io.PrintWriter
import java.io.File
import java.util.Date

object TagTree {

  def createTree(vector: List[Tag], interval: Int) = {
    val total = vector.filter{v => v.tags.size == 1}.map { v => v.count }.sum
    val tree = new Tree(new Node(new Tag(List(), Map(), total, interval, Map()), List()))
    val l1 = vector.sortBy { tag => tag.tags.size }
    
//    val subtrees = vector.par.groupBy { v => v.tags.head }.mapValues { tags => 
//      val sortedTags = tags.seq.sortBy { tag => tag.tags.size }
//      val subtree = new Tree(new Node(sortedTags.head, List()))
//      sortedTags.tail.map{ tag => subtree.insert(tag, interval) }
//      subtree
//    }
//
//    val root = tree.root
//    root.children = subtrees.values.map { subtree => subtree.root }.toList.sortBy { node => node.tag.getMaxCount() }.reverse
    
    val subtrees = createSubTree(vector, interval, 0, tree)
    tree.root.children = subtrees
    println("Habemus Tree")
    tree
  }
  
  def createSubTree(tags: List[Tag], interval: Int, level: Int, tree: Tree): List[Node] = {
    tags match {
      case Nil => List()
      case x::Nil => List(new Node(x, List()))
      case x::xs =>
        val subtrees = tags.par.filter { tag => tag.tags.size > level }.groupBy { v => v.tags.take(level + 1) }.mapValues { tags =>
          val sortedTags = tags.seq.sortBy { tag => tag.tags.size }
          val subtree = new Tree(new Node(sortedTags.head, List()))
          val subsubtree = createSubTree(sortedTags.toList.tail, interval, level + 1, subtree)
          
          val subroot = subtree.root
          subroot.children = subsubtree
          subtree
        }
        subtrees.values.map { subtree => subtree.root }.toList.sortBy { node => node.tag.getMaxCount() }.reverse
    }
  }

}

class Tree(val root: Node) {

  def insert(tag: Tag, interval: Int): Unit = {
    val sentinel = root.tag.tags.length + 1
    if (sentinel == tag.tags.length) {
      root.children = root.children :+ new Node(tag, List())
    } else if (sentinel < tag.tags.length) {
      val target = search(root, tag.tags)

      if ((target.tag.tags.length == tag.tags.length - 1)) {
        target.children = target.children :+ new Node(tag, List())
      } else { // this should never happen
        val levels = target.tag.tags.size + 1
        val node = new Node(new Tag(tag.tags.take(levels), Map(), 0, interval, Map()), List())
        target.children = node :: target.children
        insert(tag, interval)
      }

    }

  }

  def search(current: Node, key: List[String]): Node = {
    val level = current.tag.tags.length + 1
    val res = current.children.find { child => child.tag.tags == key.take(level) }
    res match {
      case Some(n) => search(n, key)
      case None => current

    }
  }
  
  def getLevel(tag: String) = {
    val firstLevel = root :: root.children
    if (tag == "") firstLevel
    else {
      val node = search(root, tag.split(" ").toList)
      node :: node.children
    }
  }

  override def toString() = root.toString()

}

case class Node(val tag: Tag, var children: List[Node]) {
  override def toString() = {
    tag.tags.mkString(" ") + " (" + tag.count + ")\n" + children.map { child => "\t" * tag.tags.length + child.toString() }.mkString("")
  }
}



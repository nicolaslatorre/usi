package database

import scala.collection.immutable.TreeMap
import java.io.PrintWriter
import java.io.File
import java.util.Date

object TagTree {
  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val mainVector = TagFactory.mainTagVector(url, username, password)
    println("Vector length: " + mainVector.size)

    val treeTag = createTree(mainVector)
    val writer = new PrintWriter(new File("../treeAll.txt"))

    writer.write(treeTag.toString())
    writer.close()

    println("First Level Size: " + treeTag.root.children.size)
    val f = treeTag.root.children
    println("Java childrens: " + f.find { node => node.tag.tags.mkString(" ") == "java" }.get.children.size)
  }

  def createTree(vector: List[Tag]) = {
    val total = vector.filter{v => v.tags.size == 1}.map { v => v.count }.sum
    val tree = new Tree(new Node(new Tag(List(), Map(), total), List()))
    val l1 = vector.sortBy { tag => tag.tags.size }

    l1.map { tag => tree.insert(tag) }
    println("Habemus Tree")
    tree
  }

}

class Tree(val root: Node) {

  def insert(tag: Tag): Unit = {
    val sentinel = root.tag.tags.length + 1
    if (sentinel == tag.tags.length) {
      root.children = root.children :+ new Node(tag, List())
    } else if (sentinel < tag.tags.length) {
      val target = search(root, tag.tags)

      if ((target.tag.tags.length == tag.tags.length - 1)) {
        target.children = target.children :+ new Node(tag, List())
      } else { // this should never happen
        val levels = target.tag.tags.size + 1
        val node = new Node(new Tag(tag.tags.take(levels), Map(), 0), List())
        target.children = node :: target.children
        insert(tag)
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



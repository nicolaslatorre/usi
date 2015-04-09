package database

import scala.collection.immutable.TreeMap
import java.io.PrintWriter
import java.io.File

object TagTree {
  def main(args: Array[String]) = {
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val mainVector = TagFactory.mainTagVector(url, username, password)
    println("Vector length: " + mainVector.size)

    val l1 = mainVector.groupBy { case (key, value) => key.split(" ").toList(0) }

    val treeTag = createTree(mainVector)
    val writer = new PrintWriter(new File("../tree.txt"))

    writer.write(treeTag.toString())
    writer.close()

  }

  def createTree(vector: List[(String, Int)]) = {
    val tree = new Tree(new Node(List(), 0, List()))
    val l1 = vector.map { case (key, value) => (key.split(" ").toList, value) }.sortBy { case (key, value) => key.size } //.filter{case(key, value) => key.size == 1}

    l1.map { case (key, value) => tree.insert(key, value) }
    println("Habemus Tree")
    tree
    
//    val l1 = vector.groupBy { case (key, value) => key.split(" ").toList(0) }
//    l1.foreach{case(key, value) => value.par.foreach{ case(k, v) => tree.insert(k.split(" ").toList, v)}}
//    println("Habemus Tree")
//    tree
  }

}

class Tree(val root: Node) {

  def insert(key: List[String], value: Int): Unit = {
    if (root.children.isEmpty) root.children = new Node(key, value, List()) :: root.children
    else {
      val sentinel = root.children.head.key
      if (sentinel.length == key.length) root.children = new Node(key, value, List()) :: root.children
      else if (sentinel.length < key.length) {
        val target = search(root, key)

        if ((target.key.length == key.length - 1)) target.children = new Node(key, value, List()) :: target.children
        else {
          val levels = target.key.size + 1
          val node = new Node(key.take(levels), 0, List())
          target.children = node :: target.children
          insert(key, value)
        }

      }
    }
  }

  def search(current: Node, key: List[String]): Node = {
    val level = current.key.length + 1
    val res = current.children.find { child => child.key == key.take(level) }
    res match {
      case Some(n) => search(n, key)
      case None => current

    }
  }

  override def toString() = root.toString()

}

case class Node(val key: List[String], val occurrences: Int, var children: List[Node]) {
  override def toString() = {
    key.mkString(" ") + " ("+occurrences+")\n" + children.map { child => "\t" * key.length + child.toString() }.mkString("")
  }
}



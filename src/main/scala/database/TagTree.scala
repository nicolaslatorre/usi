package database

import scala.collection.immutable.TreeMap

object TagTree {
  def main(args: Array[String]) = {
//    val tree: Tree[String] = Node("a", Node("b", Node("c", Empty, Empty), Node("d", Empty, Empty)), Empty)
//    println(tree)
    val url = "jdbc:postgresql://localhost:5432/stackoverflow_dump"
    val username = "sodb"
    val password = "sodb"
    val mainVector = TagFactory.mainTagVector(url, username, password)
    
    val tree = createTree(mainVector)
    
    
    
    //tree.foreach{case(key, value) => println("Key: " + key + "\tValue: " + value)}
    tree.filter{case(key, value) => key.split(" ").length == 1 }.foreach{case(key, value) => println("Key: " + key + "\tValue: " + value)}
    
  }
  
  def createTree(vector: List[(String, Int)]) = {
    val tree: TreeMap[String, Int] = new TreeMap[String, Int]
    tree ++ vector
  }
  
  def addElement(tree: TreeMap[String, Int], elem: (String, Int)) = {
    tree.+(elem)
  }

}

sealed abstract class Tree[+T]

case class Node[+T](val key: T, val left: Tree[T], val right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + key.toString + " " + left.toString + " " + right.toString + ")"
}

case object Empty extends Tree[Nothing] {
  override def toString = "."
}
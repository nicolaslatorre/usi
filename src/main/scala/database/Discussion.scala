package database

import squeryl._
import java.util.Date

class Discussion(val id: Int, val title: Option[String], val creationDate: Date, val body: String, val commentCount: Option[Int], val answerCount: Option[Int], val comments: List[Option[Comment]], val answersAndComments: Map[Post, List[Option[Comment]]], val tags: List[String]) {

  override def toString() = {
    val commentsString = {
      comments match {
        case Nil => ""
        case xs => xs.map { x =>
          x match {
            case None => ""
            case Some(n) => "\t" + n.text + "\n"
          }
        }.foldLeft("")((x, y) => x + y)
      }
    }

    val answers = {
      answersAndComments.map { x =>
        x match {
          case null => ""
          case (x, y) => x.body + "\n" + {
            y match {
              case Nil => ""
              case xs => xs.map { z =>
                z match {
                  case None => ""
                  case Some(n) => "\t" + n.text + "\n"
                }
              }.foldLeft("")((x, y) => x + y)
            }
          }
        }
      }.mkString(", ")
    }

    {
      title match {
        case None => ""
        case Some(n) => n
      }
    } + "\n" + body + "\n" + commentsString + "\n" + answers + "\n"
  }

}
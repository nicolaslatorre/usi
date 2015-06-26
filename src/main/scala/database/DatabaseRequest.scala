package database

import org.jsoup.Jsoup
import org.squeryl.PrimitiveTypeMode.date2ScalarDate
import org.squeryl.PrimitiveTypeMode.from
import org.squeryl.PrimitiveTypeMode.inTransaction
import org.squeryl.PrimitiveTypeMode.int2ScalarInt
import org.squeryl.PrimitiveTypeMode.join
import org.squeryl.PrimitiveTypeMode.optionInt2ScalarInt
import org.squeryl.PrimitiveTypeMode.optionString2ScalarString
import org.squeryl.PrimitiveTypeMode.orderByArg2OrderByExpression
import org.squeryl.PrimitiveTypeMode.queryable2JoinPrecursor
import org.squeryl.PrimitiveTypeMode.queryable2RightInnerJoinedQueryable
import org.squeryl.PrimitiveTypeMode.select
import org.squeryl.PrimitiveTypeMode.string2ScalarString
import org.squeryl.PrimitiveTypeMode.traversableOfNumericalExpressionList
import org.squeryl.PrimitiveTypeMode.typedExpression2OrderByArg
import org.squeryl.PrimitiveTypeMode.where
import org.squeryl.Session
import org.squeryl.SessionFactory
import org.squeryl.adapters.PostgreSqlAdapter

import com.github.nscala_time.time.Imports.LocalDate
import com.mchange.v2.c3p0.ComboPooledDataSource

import squeryl.Comment
import squeryl.Post
import squeryl.Post2Tag
import squeryl.StackExchangeSchema.comments
import squeryl.StackExchangeSchema.post2tag
import squeryl.StackExchangeSchema.posts

object DatabaseRequest {

  /**
   * Opens a connection with a database
   *
   * @param url the address of the database
   * @param username
   * @param password
   */
  def openConnection(url: String, username: String, password: String) = {
    val cpds = new ComboPooledDataSource()
    cpds.setDriverClass("org.postgresql.Driver") //loads the jdbc driver 
    cpds.setJdbcUrl(url)
    cpds.setUser(username)
    cpds.setPassword(password);

    Class.forName("org.postgresql.Driver");
    SessionFactory.concreteFactory = Some(() =>
      Session.create(
        cpds.getConnection(username, password),
        new PostgreSqlAdapter))

    cpds.setMaxPoolSize(50)

    println("Opened DB")
    cpds
  }

  /**
   * Retrieve the posts that are questions.
   */
  def retrieveQuestionsIds(n: Int, pageLength: Int) = {
    inTransaction {
      from(posts)(p => where(p.postTypeId === 1) select (p.id) orderBy (p.creationDate asc)).page(pageLength * n, pageLength).toSet
    }
  }

  /**
   * Retrieve the posts that are questions.
   */
  def retrieveQuestionsIdsInInterval(start: LocalDate, end: LocalDate, tags: List[String]) = {
    val sDate = start.toDate()
    val eDate = end.toDate()
    val tag = tags.map { tag => "&lt;" + tag + "&gt;" }.mkString("")
    inTransaction {
      from(posts)(p => where((p.postTypeId === 1) and (p.creationDate >= sDate and p.creationDate < eDate) and (p.tags like tag)) select (p.id) orderBy (p.creationDate asc)).toSet
    }
  }

  /**
   * Retrieve all the posts that are questions.
   */
  def retrieveQuestionsIds() = {
    inTransaction {
      from(posts)(p => where(p.postTypeId === 1) select (p.id) orderBy (p.creationDate asc))
    }.toSet
  }

  /**
   * Retrieve the posts in a specific set of ids
   */
  def retrieveSpecificIds(n: Int, pageLength: Int, keyword: String) = {
    inTransaction {
      from(posts)(p => where((p.postTypeId === 1) and (p.tags like keyword)) select (p.id) orderBy (p.creationDate asc)).page(n, pageLength)
      //from(posts)(p => where((p.postTypeId === 1)) select(p.id)).page(n, pageLength)
    }.toSet
  }

  /**
   * Retrieve questions with respective comments
   */
  def retrieveQuestionsAndComments(ids: Set[Int]) = {
    inTransaction {
      val questionsAndAnswers = join(posts, comments.leftOuter)((p, c) =>
        where(p.id in ids)
          select (p, c)
          on (p.id === c.map(_.postId)))
      mapPostsWithComments(questionsAndAnswers.toList)
    }.toMap
  }

  /**
   * Retrieve a post by tags
   */
  def retrieveIdsByTag(tags: List[String]) = {
    inTransaction {
      val ts = tags.map { tag => "&lt;" + tag + "&gt;" }.mkString("")
      from(posts)(p => where(p.tags like ts) select (p.id)).toList
    }
  }

  /**
   *  Retrieve questions informations by ids
   */
  def retrieveQuestionsInfoByIds(ids: Set[Int]) = {
    inTransaction {
      val result = from(posts)(p => where((p.postTypeId === 1) and (p.id in ids))
        select (p.id, p.creationDate, p.title, p.tags, p.score, p.viewCount, p.ownerId, p.answerCount))

      result.par.map {
        case (id, creation, title, tags, score, view, owner, answers) =>
          val t = title match {
            case None => "NO TITLE"
            case Some(n) => n
          }

          val own = owner match {
            case None => -1
            case Some(o) => o
          }

          val ans = answers match {
            case None => 0
            case Some(a) => a
          }

          val tagsList = tags match {
            case None => Nil
            case Some(t) => parseHtml(t).replace("<", "").replace(">", " ").split(" ").toList
          }

          new Discussion(id, t, creation, ans, score, view, own, tagsList)
      }.toList
    }

  }

  /**
   *  Retrieve questions informations
   */
  def retrieveQuestionsInfo() = {
    inTransaction {
      val result = from(posts)(p => where((p.postTypeId === 1))
        select (p.id, p.creationDate, p.title, p.tags, p.score, p.viewCount, p.ownerId, p.answerCount) orderBy (p.creationDate))

      result.par.map {
        case (id, creation, title, tags, score, view, owner, answers) =>
          val t = title match {
            case None => "NO TITLE"
            case Some(n) => n
          }

          val own = owner match {
            case None => -1
            case Some(o) => o
          }

          val ans = answers match {
            case None => 0
            case Some(a) => a
          }

          val tagsList = tags match {
            case None => Nil
            case Some(t) => parseHtml(t).replace("<", "").replace(">", " ").split(" ").toList
          }

          new Discussion(id, t, creation, ans, score, view, own, tagsList)
      }.toList
    }

  }

  /**
   * Retrieve answers with respective comments
   */
  def retrieveAnswersAndComments(ids: Set[Int]) = {
    inTransaction {
      val answersAndComments = join(posts, comments.leftOuter)((p, c) =>
        where(((p.parentId in ids)) and (p.postTypeId === 2))
          select (p, c)
          on (p.id === c.map(_.postId)))
      mapPostsWithComments(answersAndComments.toList)
    }.toMap
  }

  /**
   * Map posts with comments
   */
  def mapPostsWithComments(postsAndComments: List[(Post, Option[Comment])]) = {
    val posts = postsAndComments.map { case (post, comment) => (post.id, post) }.toMap
    val idsAndComments = postsAndComments.groupBy {
      case (post, comment) => post.id
    }.mapValues {
      pc => pc.flatMap { case (post, comment) => comment }
    }
    idsAndComments.map { case (id, comments) => (posts.get(id).get, comments) }
  }

  /**
   * Retrieve Tags
   */
  def retrieveTagsPosts(ids: Set[Int]) = {
    inTransaction {
      val tags = from(posts)(p => where((p.id in ids) and (p.postTypeId === 1)) select (p.id, p.tags)).toMap
      tags.map {
        case (id, tags) =>
          val ts = Jsoup.parse(tags.get).body().text().replace("<", "").replace(">", " ").split(" ").toList
          (id, ts)
      }
    }
  }

  /**
   * Insert tags in table
   */
  def insertTags(postsTags: Map[Int, List[String]]) = {
    inTransaction {
      val p2t = postsTags.map { case (id, tags) => new Post2Tag(id, tags.mkString(" ")) }.toList
      println("Ready to insert")
      post2tag.insert(p2t)
    }
  }

  /**
   * Retrieve tag2post
   */
  def retrieveTag2Post() = {
    inTransaction {
      from(post2tag)(pt => select(pt.id, pt.tags)) //.page(0, 500000)
    }.toMap.map { case (id, tags) => (id, tags.split(" ").toList) }
  }

  def retrieveTag2PostWithDate(n: Int, pageLength: Int) = {
    inTransaction {
      val res = join(posts, post2tag)((p, pt) =>
        where((p.postTypeId === 1))
          select (pt, p.creationDate) orderBy (p.creationDate)
          on (p.id === pt.id)).page(n, pageLength).toList

      println(res.size)
      res
    }
  }

  /**
   * Retrieve all elements from tag2post corresponding to the given tag
   */
  def retrieveTag2PostWithTag(n: Int, pageLength: Int, tags: List[String]) = {
    val ts = tags.map { tag => "&lt;" + tag + "&gt;" }.mkString("")
    inTransaction {
      join(posts, post2tag)((p, pt) =>
        where((p.postTypeId === 1) and (p.tags like ts))
          select (pt, p.creationDate) orderBy (p.creationDate)
          on (p.id === pt.id)).page(n, pageLength).toList
    }
  }

  /**
   * Retrieve all elements from tag2post corresponding to the given tag
   */
  def retrieveTag2PostWithTagInInterval(n: Int, pageLength: Int, tags: List[String], startDate: LocalDate, interval: Int) = {
    val ts = tags.map { tag => "&lt;" + tag + "&gt;" }.mkString("")
    inTransaction {
      join(posts, post2tag)((p, pt) =>
        where((p.postTypeId === 1) and (p.tags like ts) and (p.creationDate >= startDate.toDate) and (p.creationDate < startDate.plusDays(interval).toDate))
          select (pt, p.title) orderBy (p.creationDate)
          on (p.id === pt.id)).page(n, pageLength).toList
    }
  }

  /**
   * Get occurrences of a tag
   */
  def getTagOccurrences(tag: String) = {
    val occurrence = inTransaction {
      from(posts)(p => where((p.postTypeId === 1) and (p.tags like "%&lt;" + tag + "&gt;%")) select (p.id)).toList.size
    }
    (tag, occurrence)
  }

  /**
   * Build tags as a list of strings
   */
  def buildTags(tags: String) = {
    parseHtml(tags).replace("<", "").replace(">", " ").split(" ").toList
  }

  def parseHtml(text: String) = {
    Jsoup.parse(text).body().text()
  }
}
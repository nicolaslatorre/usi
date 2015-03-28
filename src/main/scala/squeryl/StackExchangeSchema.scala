package squeryl

import org.squeryl.Schema
//import org.apache.lucene.search.TermQuery


object StackExchangeSchema extends Schema {

  val posts = table[Post]("posts") 
  val comments = table[Comment]("comments")
  val terms = table[Dictionary]("dictionary")
  val contextVectors = table[ContextVector]("context_vectors")
  val users = table[User]("users")
  val votes = table[Vote]("votes")
  val postsHistory = table[PostsHistory]("posts_history")
  val badges = table[Badge]("badges")
//  val termsEntropy = table[TermEntropy]("terms_entropy")
//  val postsMetric = table[PostMetric]("posts_metrics")
  val badPosts = table[BadPosts]("bad_posts")
  val goodPosts = table[GoodPosts]("good_posts")
//  val userVotes = table[UserVote]("user_votes")
//  val trainingData = table[TrainingData]("training_data")
//  val stackOverflowPostQuality = table[StackOverflowPostQuality]("post_quality")
}
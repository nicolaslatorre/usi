package topicmodeling

import java.io.File
import java.util.regex.Pattern
import scala.collection.JavaConversions.asJavaCollection
import com.github.tototoshi.csv.CSVWriter
import cc.mallet.pipe.CharSequence2TokenSequence
import cc.mallet.pipe.CharSequenceLowercase
import cc.mallet.pipe.SerialPipes
import cc.mallet.pipe.TokenSequence2FeatureSequence
import cc.mallet.pipe.TokenSequenceRemoveStopwords
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.Alphabet
import cc.mallet.types.Instance
import cc.mallet.types.InstanceList
import cc.mallet.pipe.iterator.CsvIterator
import com.github.tototoshi.csv.CSVReader
import cc.mallet.pipe.TokenSequenceRemoveNonAlpha
import scala.io.Source

object TopicModeling {

  def main(args: Array[String]) = {
    val openPath = "datasets/dataset7/discussions-nocomment.csv"
    
    
    val alpha = 10.0
    val beta = 0.006
    val iterations = 1000
    val discussions = getDiscussions(openPath)
    val numberOfTopics = 4
    val instanceList = createInstanceList(discussions)

    val threads = Runtime.getRuntime.availableProcessors()
    val model = executeLDA(instanceList, numberOfTopics, alpha, beta, threads, iterations)

    val instancesRange = (0 until instanceList.size())
    val probabilities = instancesRange.map { index => wrapProbabilities(model, index) }.toList

    println(probabilities.size)

    val topicSequences = model.getData.get(0).topicSequence
    val distribution = model.getTopicProbabilities(topicSequences).toList
    println(distribution.length)
    
    val resultPath = "datasets/dataset7/document-distribution-" + numberOfTopics + "-questions.csv"
    writeResult(probabilities, numberOfTopics, discussions, resultPath)

    println()

  }
  
  def getNumberOfTags(discussions: List[List[String]]) = {
    discussions.flatMap { x => x(3).split(" ").toList }.distinct.size
    //println(tags)
    //tags.size
  }

  //MyClass should be the same

  //symbols/numbers
  //Lowercase
  //Filtering length >= 3
  //StopWords, tutte in lowercase
  //stemming, porter stemmer or snowball stemmer

  def getDiscussions(path: String) = {
    val reader = CSVReader.open(new File(path))
    reader.all()
  }

  def createInstanceList(discussions: List[List[String]]) = {
    val pattern = Pattern.compile("\\S+")

    val pipeList = List(new CharSequenceLowercase, new CharSequence2TokenSequence(), new TokenSequenceRemoveNonAlpha, new TokenSequenceFilterLength(3),
      getStopWords, new Stemming, new TokenSequence2FeatureSequence())
    val pipes = new SerialPipes(pipeList)

    val instanceList = new InstanceList(pipes)

    val instances = discussions.map { xs => instanceList.addThruPipe(createInstance(xs)) }

    instanceList
  }

  def getStopWords() = {
    val stopwords = new File("stoplist/stopwords.txt")

    val stopwords2 = new File("stoplist/stopwordsPython.txt")
    val programmingStopwords = Source.fromFile(stopwords2).mkString.split("\n")

    val removeStopWords = new TokenSequenceRemoveStopwords(stopwords, "UTF-8", false, false, false)
    removeStopWords.addStopWords(stopwords2)
    removeStopWords
  }

  def createInstance(line: List[String]) = {
    val Array(id, label, text, tags) = line.toArray
    new Instance(text, "noLabel", id, 0) // the last value of the instance constructor is for classification task, it is not needed here.
  }

  def executeLDA(instanceList: InstanceList, numberOfTopics: Int, alpha: Double, beta: Double, numberOfThreads: Int, numberOfIterations: Int) = {
    val model = new ParallelTopicModel(numberOfTopics, alpha, beta)
    model.addInstances(instanceList)
    model.setNumThreads(numberOfThreads)
    model.setNumIterations(numberOfIterations)
    model.estimate()
    model
  }

  def getTopWords(model: ParallelTopicModel, numberOfWords: Int) = {
    val topWords = model.getTopWords(numberOfWords)
    topWords.map { x => x.map { y => y.asInstanceOf[String] } }
  }

  def wrapAlphabet(alphabet: Alphabet) = {
    alphabet.toArray.toList.asInstanceOf[List[String]]
  }

  def wrapProbabilities(model: ParallelTopicModel, index: Int) = {
    val probabilities = model.getTopicProbabilities(index)
    val names = model.getData
    val wrappedNames = model.getData.get(index).instance.getName.asInstanceOf[String]
    val wrappedProbabilities = (for (i <- 0 to (probabilities.length - 1)) yield probabilities(i)).toList

    wrappedNames -> wrappedProbabilities
  }

  def writeResult(probabilities: List[(String, List[Double])], numberOfTopics: Int, discussions: List[List[String]], path: String) = {
    val firstRow = (for (i <- 0 until numberOfTopics) yield "T" + i).toList
    val file = new File(path)
    val writer = CSVWriter.open(file)

    val ds = discussions.map { xs => (xs(0), xs(2), xs(3)) }

    writer.writeRow("" :: (firstRow ::: List("Text")))
    //    probabilities.foreach { case (x, ys) => writer.writeRow(x :: ys.map { y => y.toString() }) }
    //val map = (ds zip probabilities).groupBy(_._1).mapValues(x => (x(0)._1._2, x(0)._2._2)).map { case (x, y) => (x._1, y) }
    val map = (ds zip probabilities).groupBy(_._1).mapValues{x => 
      val text = x(0)._1._2
      val tags = x(0)._1._3
      val probabilities = x(0)._2._2
      (text, tags, probabilities)
      }.map{case(x, y) => (x._1, y)}
    
    
    map.foreach {
      case (x, y) =>
        writer.writeRow(x :: y._1 :: y._2 :: y._3.map { x => x.toString() })
    }

    writer.close()
  }

}
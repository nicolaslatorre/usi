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
    val path = "discussions.old.csv" // 904 discussions, 200 discussions
    val numberOfTopics = 50
    val alpha = 1.0 / numberOfTopics
    val beta = 0.006
    val iterations = 1000
    val instanceList = createInstanceList(path)

    val threads = Runtime.getRuntime.availableProcessors()
    val model = executeLDA(instanceList, numberOfTopics, alpha, beta, threads, iterations)

    val instancesRange = (0 until (instanceList.size() - 1))
    val probabilities = instancesRange.map { index => wrapProbabilities(model, index) }.toList

    println(probabilities.size)
    
    val topicSequences = model.getData.get(0).topicSequence
    val distribution = model.getTopicProbabilities(topicSequences).toList
    println(distribution.length)
    writeResult(probabilities, numberOfTopics)

    //topWords.foreach { x => x.foreach { y => println(y)} }

    println()

  }

  //MyClass should be the same

  //symbols/numbers
  //Lowercase
  //Filtering length >= 3
  //StopWords, tutte in lowercase
  //stemming, porter stemmer or snowball stemmer

  def createInstanceList(path: String) = {
    //val pattern = Pattern.compile("\\p{L}[\\p{L}\\p{P}]+\\p{L}")
    val reader = CSVReader.open(new File(path))
    val discussions = reader.all()

    val pattern = Pattern.compile("\\S+")

    val pipeList = List(new CharSequenceLowercase, new CharSequence2TokenSequence(pattern), new TokenSequenceFilterLength(3), new TokenSequenceRemoveNonAlpha,
      getStopWords, new Stemming, getStopWords, new TokenSequence2FeatureSequence())
    val pipes = new SerialPipes(pipeList)

    val instanceList = new InstanceList(pipes)

    val instances = discussions.map { xs => instanceList.addThruPipe(createInstance(xs)) }

    instanceList
  }

  def getStopWords() = {
    val stopwords = new File("stoplist/stop-words_english_1_en.txt")

    val stopwords2 = new File("stoplist/stopwordsProgramming.txt")
    val programmingStopwords = Source.fromFile(stopwords2).mkString.split("\n")

    val removeStopWords = new TokenSequenceRemoveStopwords(stopwords, "UTF-8", false, false, false)
    removeStopWords.addStopWords(stopwords2)
    removeStopWords
  }

  def createInstance(line: List[String]) = {
    val Array(id, label, text) = line.toArray
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

  def writeResult(probabilities: List[(String, List[Double])], numberOfTopics: Int) = {
    val firstRow = (for (i <- 0 to (numberOfTopics - 1)) yield "T" + i).toList
    val path = "document-distribution.csv"
    val file = new File(path)

    val writer = CSVWriter.open(file)
    writer.writeRow("" :: firstRow)
    probabilities.foreach { case (x, ys) => writer.writeRow(x :: ys.map { y => y.toString() }) }
    writer.close()
  }

}
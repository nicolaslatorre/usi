package lucene

import java.io.Reader

import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.core._
import org.apache.lucene.analysis.miscellaneous._
import org.apache.lucene.analysis.miscellaneous.WordDelimiterFilter._
import org.apache.lucene.analysis.snowball.SnowballFilter
import org.apache.lucene.analysis.standard._
import org.apache.lucene.analysis.util.CharArraySet
import org.apache.lucene.analysis.util.StopwordAnalyzerBase
import org.apache.lucene.analysis.util.WordlistLoader
import org.apache.lucene.util.Version


class DefaultLuceneAnalyzer(matchVersion: Version, stopWords: CharArraySet) extends StopwordAnalyzerBase(matchVersion, stopWords){

  def this(matchVersion: Version) = {
    this(matchVersion, StopAnalyzer.ENGLISH_STOP_WORDS_SET)
  }
  
  
  def this(matchVersion: Version, stopwords: Reader) = {
    this(matchVersion, WordlistLoader.getWordSet(stopwords, matchVersion))
  }
  
  
  val DEFAULT_MAX_TOKEN_LENGTH = 255
  val maxTokenLength = DEFAULT_MAX_TOKEN_LENGTH;
  
 
  def createComponents(fieldName: String, reader: Reader) = {
    val src = new StandardTokenizer(matchVersion, reader)
    src.setMaxTokenLength(maxTokenLength)
    var tok: TokenStream = new StandardFilter(matchVersion, src)
    tok = new WordDelimiterFilter(tok, GENERATE_WORD_PARTS | SPLIT_ON_CASE_CHANGE | SPLIT_ON_NUMERICS, CharArraySet.EMPTY_SET)
    tok = new LengthFilter(tok,3,20)
    tok = new NumericFilter(matchVersion, tok)
    tok = new LowerCaseFilter(matchVersion, tok)
    tok = new StopFilter(matchVersion, tok, stopwords)
    tok = new SnowballFilter(tok, "English")
    new TokenStreamComponents(src, tok) {
     override def setReader(reader: Reader){
        src.setMaxTokenLength(DefaultLuceneAnalyzer.this.maxTokenLength);
        super.setReader(reader);
      }
    }
  }
}
package lucene

import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.util.FilteringTokenFilter
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute

class NumericFilter(version : Version, input : TokenStream) extends FilteringTokenFilter(input) {

  private val termAtt = addAttribute(classOf[CharTermAttribute])
  
  override def accept() = {
	  val buffer = termAtt.buffer()
    val length = termAtt.length()
    val term = buffer.grouped(length).next()
    !term.forall(c => c.isDigit)
  }
  
  
  
}
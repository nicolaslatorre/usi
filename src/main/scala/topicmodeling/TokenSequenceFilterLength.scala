package topicmodeling

import cc.mallet.types.TokenSequence
import cc.mallet.types.Instance
import cc.mallet.pipe.Pipe

class TokenSequenceFilterLength(val value: Int) extends Pipe {

  override def pipe(carrier: Instance) = {
    val tokenSequence = carrier.getData.asInstanceOf[TokenSequence]
    val filteredTokenSequence = new TokenSequence()

    val indexes = 0 until tokenSequence.size()

    indexes.foreach { x =>
      val token = tokenSequence.get(x)
      val length = token.getText.length()
      if (length >= value) filteredTokenSequence.add(token)
    }

    carrier.setData(filteredTokenSequence)
    carrier
  }
}
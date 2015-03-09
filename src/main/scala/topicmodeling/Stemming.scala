package topicmodeling

import cc.mallet.pipe.Pipe
import org.tartarus.snowball._
import cc.mallet.types.Instance
import cc.mallet.types.TokenSequence
import org.tartarus.snowball.ext._

class Stemming extends Pipe {

  override def pipe(carrier: Instance) = {

    val stemmer = new englishStemmer()

    val tokenSequence = carrier.getData.asInstanceOf[TokenSequence]

    val indexes = 0 until tokenSequence.size()

    indexes.foreach { x =>
      val token = tokenSequence.get(x)
      stemmer.setCurrent(token.getText)
      stemmer.stem()
      token.setText(stemmer.getCurrent)
    }

    carrier
  }

}
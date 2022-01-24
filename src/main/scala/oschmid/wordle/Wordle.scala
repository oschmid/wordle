package oschmid.wordle

import scala.collection.mutable.{Map => MMap}

object Wordle {

  /**
   * Takes in the corpus, the guessed word, and the result in the form of a length-5 string: 0 for grey, 1 for yellow, 2 for green.
   * Returns the corpus filtered for only the words that satisfy that query.
   */
  def constrain(corpus : Seq[String], query : String, result : String): Seq[String] = corpus.filter(w => guess(query, w) == result)

  def guess(query : String, answer : String): String = {
    val letters = countmap(answer)
    var result = "00000"
    for (i <- 0 until 5) {
      if (answer.charAt(i) == query.charAt(i)) {
        result = result.patch(i, "2", 1)
        letters.updateWith(query.charAt(i)) { o => o.map {_ - 1 }}
      }
    }
    for (i <- 0 until 5) {
      if (answer.contains(query.charAt(i)) && result.charAt(i) != '2' && letters.getOrElse(query.charAt(i), 0) > 0) {
        result = result.patch(i, "1", 1)
        letters.updateWith(query.charAt(i)) { o => o.map {_ - 1 }}
      }
    }
    result
  }

  def countmap[T](s: Seq[T]): MMap[T, Int] = {
    val m = MMap[T, Int]()
    m ++= s.groupBy(identity).view.mapValues(_.length).toMap
    m
  }
}

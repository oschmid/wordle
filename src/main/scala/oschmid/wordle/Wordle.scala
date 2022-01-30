package oschmid.wordle

import scala.collection.mutable.{Map => MMap}
import scala.math.log10

object Wordle {

  /**
   * Maximises (a slight approximation to) the differential entropy of the guesses over the corpus.
   * Takes in the corpus and returns the element of Possible.words that provides the best split across it.
   */
  def choose(corpus : Seq[String]): String = {
    if (corpus.length == 1) {
      return corpus.head
    }
    var wordSoFar = corpus.head
    val hasCharInPos = Array(MMap[Char, Int](), MMap[Char, Int](), MMap[Char, Int](), MMap[Char, Int](), MMap[Char, Int]())
    for (word <- corpus) {
      for (i <- 0 until 5) {
        if (word.charAt(i) != wordSoFar.charAt(i)) {
          wordSoFar = wordSoFar.patch(i, "0", 1)
        }
        hasCharInPos(i).updateWith(word.charAt(i)) { case None => Some(1); case Some(n) => Some(n + 1) }
      }
    }

    var bestWord = ""
    var bestDiffEntropy : Double = 0
    for (word <- Possible.words) {
      var diffEntropy : Double = 0
      var seenChars = Set[Char]()
      for (i <- 0 until 5) {
        val char = word.charAt(i)
        val greens = hasCharInPos(i).getOrElse(char, 0)
        var yellows = 0
        if (!seenChars.contains(char)) {
          // a yellow check for a double letter isn't useful
          // not strictly true, but good enough for now
          for (j <- 0 until 5) {
            if (wordSoFar.charAt(j) != char) {
              yellows += hasCharInPos(j).getOrElse(char, 0)
            }
          }
          yellows -= 0
          seenChars = seenChars.incl(char)
        }
        val greys = corpus.length - yellows - greens
        val total = (greens + yellows + greys).toDouble
        val dist = Array[Double](greens / total, yellows / total, greys / total)
        diffEntropy += entropy(dist)
      }
      if (diffEntropy > bestDiffEntropy) {
        bestDiffEntropy = diffEntropy
        bestWord = word
      }
    }
    bestWord
  }

  def entropy(dist : Array[Double]): Double = {
    - dist.map(e => e * log2Safe(e)).sum
  }

  def log2Safe(x : Double): Double = {
    if (x > 0) log10(x) / log10(2.0) else 0
  }

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

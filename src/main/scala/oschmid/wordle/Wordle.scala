package oschmid.wordle

import scala.collection.mutable.{Map => MMap}

object Wordle {

  def countmap[T](s: Seq[T]): MMap[T, Int] = {
    val m = MMap[T, Int]()
    m ++= s.groupBy(identity).view.mapValues(_.length).toMap
    m
  }
}

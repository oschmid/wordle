package oschmid.wordle

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api._
import scala.collection.mutable.{Map => MMap}

class WordleTest {

  @Test def testCountmap(): Unit = {
    val m = Wordle.countmap("elegy")
    val expected = MMap("e" -> 2, "l" -> 1, "g" -> 1, "y" -> 1)
    assertEquals(expected.size, m.size)
    for ((k, v) <- expected) {
      assertEquals(Some(v), m.get(k.charAt(0)))
    }
  }

  @Test def testConstrain(): Unit = {
    val r = Wordle.constrain(Seq("soare", "arise"), "soare", "10112")
    assertEquals(Seq("arise"), r)
  }

  @Test def testChoose(): Unit = {
    val bestFirstGuess = Wordle.choose(Possible.answers)
    assertEquals("soare", bestFirstGuess)
  }
}

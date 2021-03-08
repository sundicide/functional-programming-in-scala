package lesson5

import org.junit._
import org.junit.Assert.assertEquals

class StreamSuite {
  trait TestSets {
    val n3: Stream[Int] = Stream(1,2,3)
    val n5: Stream[Int] = Stream(1,2,3,4,5)
  }

  @Test def `toList test`(): Unit = {
    new TestSets {
      assertEquals(Stream(1,2,3).toList, List(1,2,3))
    }
  }

  @Test def take(): Unit = {
    new TestSets {
      assertEquals(n5.take(4).toList, List(1,2,3,4))
    }
  }

  @Test def drop(): Unit = {
    new TestSets {
      assertEquals(n5.drop(2).toList, List(3,4,5))
    }
  }

  @Test def takeWhile(): Unit = {
    new TestSets {
      assertEquals(n5.takeWhile(n => n == 3).toList, List(3))
      assertEquals(n5.takeWhile(n => n == 3 || n == 5).toList, List(3, 5))
      assertEquals(n5.takeWhile(n => List(2,3,4).contains(n)).toList, List(2, 3, 4))
    }
  }

}
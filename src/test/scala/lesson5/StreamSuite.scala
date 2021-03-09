package lesson5

import org.junit._
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

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
      assertEquals(n5.takeWhile(n => n == 3).toList, List(1, 2, 3))
    }
  }

  @Test def forAll(): Unit = {
    new TestSets {
      assertFalse(n5.forAll(n => n == 3))
      assertFalse(n5.forAll(n => n5.toList.contains(n)))
    }
  }

  @Test def takeWhile2(): Unit = {
    new TestSets {
      assertEquals(n5.takeWhile2(n => n == 3).toList, List(1, 2, 3))
    }
  }

  @Test def `ex5.8) constant`: Unit = {
    new TestSets {
      println(Stream.constant(3).take(5).toList)
      assertTrue(Stream.constant(3).exists(_ == 3))
//      assertFalse(Stream.constant(3).exists(_ == 4))
    }
  }

  @Test def `ex5.9) from`: Unit = {
    new TestSets {
      println(Stream.from(3).take(5).toList)
      assertEquals(Stream.from(3).take(2).toList, List(3,4))
      //      assertFalse(Stream.constant(3).exists(_ == 4))
    }
  }
}
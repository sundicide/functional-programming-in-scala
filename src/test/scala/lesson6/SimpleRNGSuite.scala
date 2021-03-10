package lesson6

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit._

class SimpleRNGSuite {
  trait TestSets {
//    val n3: Stream[Int] = Stream(1,2,3)
//    val n5: Stream[Int] = Stream(1,2,3,4,5)
  }

  @Test def `RNG test`(): Unit = {
    val rng = SimpleRNG(42)
    assertEquals(rng, SimpleRNG(42))

    val (n1, rng2) = rng.nextInt
    assertEquals(n1, 16159453)
    assertEquals(rng2, SimpleRNG(1059025964525L))

    val (n2, rng3) = rng2.nextInt
    assertEquals(n2, -1281479697)
    assertEquals(rng3, SimpleRNG(197491923327988L))
  }
}